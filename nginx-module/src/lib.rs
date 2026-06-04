//! nginx-модуль `ngx_http_lox_module`.
//!
//! Добавляет директиву `lox_guard '<выражение>;'` для `location`. На access-фазе
//! выражение на языке lox вычисляется с контекстом запроса; если результат
//! «истинный» — запрос пропускается дальше (`NGX_DECLINED`), иначе возвращается
//! `403 Forbidden`.
//!
//! В окружение guard-а инжектируются:
//! - переменная `uri`     — путь запроса (например `/admin`);
//! - переменная `method`  — HTTP-метод (`GET`, `POST`, ...);
//! - функция  `header(name)` — значение заголовка запроса или `nil`.

use std::collections::HashMap;
use std::ffi::{c_char, c_void};
use std::rc::Rc;

use lox::interpreter::{Interpreter, LoxCallable};
use lox::Value;

use ngx::core;
use ngx::ffi::{
    ngx_array_push, ngx_command_t, ngx_conf_t, ngx_http_handler_pt, ngx_http_module_t,
    ngx_http_phases_NGX_HTTP_ACCESS_PHASE, ngx_int_t, ngx_module_t, ngx_str_t, ngx_uint_t,
    NGX_CONF_TAKE1, NGX_HTTP_LOC_CONF, NGX_HTTP_LOC_CONF_OFFSET, NGX_HTTP_MODULE, NGX_LOG_EMERG,
};
use ngx::http::{
    self, HttpModule, HttpModuleLocationConf, HttpModuleMainConf, MergeConfigError,
    NgxHttpCoreModule,
};
use ngx::{http_request_handler, ngx_conf_log_error, ngx_log_debug_http, ngx_string};

struct Module;

impl http::HttpModule for Module {
    fn module() -> &'static ngx_module_t {
        unsafe { &*::core::ptr::addr_of!(ngx_http_lox_module) }
    }

    unsafe extern "C" fn postconfiguration(cf: *mut ngx_conf_t) -> ngx_int_t {
        // SAFETY: cf всегда не-NULL при вызове из nginx.
        let cf = &mut *cf;
        let cmcf = NgxHttpCoreModule::main_conf_mut(cf).expect("http core main conf");

        let h = ngx_array_push(
            &mut cmcf.phases[ngx_http_phases_NGX_HTTP_ACCESS_PHASE as usize].handlers,
        ) as *mut ngx_http_handler_pt;
        if h.is_null() {
            return core::Status::NGX_ERROR.into();
        }
        *h = Some(lox_guard_handler);
        core::Status::NGX_OK.into()
    }
}

#[derive(Debug, Default)]
struct ModuleConfig {
    /// Исходный текст guard-выражения из директивы `lox_guard`.
    guard: Option<String>,
}

unsafe impl HttpModuleLocationConf for Module {
    type LocationConf = ModuleConfig;
}

static mut NGX_HTTP_LOX_COMMANDS: [ngx_command_t; 2] = [
    ngx_command_t {
        name: ngx_string!("lox_guard"),
        type_: (NGX_HTTP_LOC_CONF | NGX_CONF_TAKE1) as ngx_uint_t,
        set: Some(ngx_http_lox_set_guard),
        conf: NGX_HTTP_LOC_CONF_OFFSET,
        offset: 0,
        post: std::ptr::null_mut(),
    },
    ngx_command_t::empty(),
];

static NGX_HTTP_LOX_MODULE_CTX: ngx_http_module_t = ngx_http_module_t {
    preconfiguration: Some(Module::preconfiguration),
    postconfiguration: Some(Module::postconfiguration),
    create_main_conf: None,
    init_main_conf: None,
    create_srv_conf: None,
    merge_srv_conf: None,
    create_loc_conf: Some(Module::create_loc_conf),
    merge_loc_conf: Some(Module::merge_loc_conf),
};

// Таблица `ngx_modules` для сборки cdylib вне buildsystem nginx.
#[cfg(feature = "export-modules")]
ngx::ngx_modules!(ngx_http_lox_module);

#[used]
#[allow(non_upper_case_globals)]
#[cfg_attr(not(feature = "export-modules"), no_mangle)]
pub static mut ngx_http_lox_module: ngx_module_t = ngx_module_t {
    ctx: std::ptr::addr_of!(NGX_HTTP_LOX_MODULE_CTX) as _,
    commands: unsafe { &NGX_HTTP_LOX_COMMANDS[0] as *const _ as *mut _ },
    type_: NGX_HTTP_MODULE as _,
    ..ngx_module_t::default()
};

impl http::Merge for ModuleConfig {
    fn merge(&mut self, prev: &ModuleConfig) -> Result<(), MergeConfigError> {
        // Наследуем guard из внешнего location, если в текущем он не задан.
        if self.guard.is_none() {
            self.guard = prev.guard.clone();
        }
        Ok(())
    }
}

/// native-функция lox `header("name")` → значение заголовка запроса или `nil`.
/// Заголовки собраны заранее (запрос жив всё время вычисления guard-а),
/// поэтому unsafe и сырые указатели здесь не нужны.
struct HeaderFn {
    headers: Rc<HashMap<String, String>>,
}

impl LoxCallable for HeaderFn {
    fn arity(&self) -> usize {
        1
    }

    fn name(&self) -> &str {
        "header"
    }

    fn call(&self, _interp: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
        match args.into_iter().next() {
            Some(Value::String(name)) => Ok(self
                .headers
                .get(&name.to_ascii_lowercase())
                .map(|v| Value::String(v.clone()))
                .unwrap_or(Value::Nil)),
            _ => Err("header() expects a string argument".into()),
        }
    }
}

http_request_handler!(lox_guard_handler, |request: &mut http::Request| {
    let co = Module::location_conf(request).expect("module config is none");

    let Some(src) = co.guard.clone() else {
        // Директива в этом location не задана — не вмешиваемся.
        return core::Status::NGX_DECLINED;
    };

    let uri = request.path().to_str().unwrap_or("").to_string();
    let method = request.method().as_str().to_string();

    // Собираем заголовки запроса в map с ключами в нижнем регистре.
    let mut headers: HashMap<String, String> = HashMap::new();
    for (key, value) in request.headers_in_iterator() {
        if let (Ok(k), Ok(v)) = (key.to_str(), value.to_str()) {
            headers.insert(k.to_ascii_lowercase(), v.to_string());
        }
    }
    let headers = Rc::new(headers);

    let result = lox::eval_guard(&src, |interp| {
        interp.define_global("uri", Value::String(uri));
        interp.define_global("method", Value::String(method));
        interp.define_global(
            "header",
            Value::NativeFunction(Rc::new(HeaderFn { headers })),
        );
    });

    match result {
        Ok(true) => {
            ngx_log_debug_http!(request, "lox_guard: allow");
            core::Status::NGX_DECLINED
        }
        Ok(false) => {
            ngx_log_debug_http!(request, "lox_guard: deny (403)");
            http::HTTPStatus::FORBIDDEN.into()
        }
        Err(e) => {
            ngx_log_debug_http!(request, "lox_guard error: {}", e);
            http::HTTPStatus::FORBIDDEN.into()
        }
    }
});

/// Setter директивы `lox_guard '<выражение>;'` — сохраняет текст в location-конфиг.
extern "C" fn ngx_http_lox_set_guard(
    cf: *mut ngx_conf_t,
    _cmd: *mut ngx_command_t,
    conf: *mut c_void,
) -> *mut c_char {
    unsafe {
        let conf = &mut *(conf as *mut ModuleConfig);
        let args: &[ngx_str_t] = (*(*cf).args).as_slice();

        match args[1].to_str() {
            Ok(s) => conf.guard = Some(s.to_string()),
            Err(_) => {
                ngx_conf_log_error!(NGX_LOG_EMERG, cf, "`lox_guard` argument is not utf-8 encoded");
                return ngx::core::NGX_CONF_ERROR;
            }
        }
    };

    ngx::core::NGX_CONF_OK
}
