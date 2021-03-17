#![macro_use]

#[allow(unused_macros)]
//#[macro_use]
#[macro_export]
macro_rules! do_parse {
    ($x:pat =o $e:expr, =o $exp:expr) => {
        $e.and_then(move |$x| $exp)
    };
    ($x:pat =o $e:expr, $($y:pat =o $es:expr),+, =o $exp:expr) => {
        $e.and_then(move |$x| do_parse!($($y =o $es),+, =o $exp))
    };
}
