#[macro_use] extern crate nickel;
extern crate rustc_serialize;

use std::collections::BTreeMap;

use nickel::{
    Nickel, Request, HttpRouter
};

use rustc_serialize::json::{Json, ToJson};

#[derive(RustcDecodable, RustcEncodable)]
struct Res {
    res: String,
}

impl ToJson for Res {
    fn to_json(&self) -> Json {
        let mut map = BTreeMap::new();
        map.insert("pong".to_string(), self.res.to_json());
        Json::Object(map)
    }
}


fn main() {
    let mut server = Nickel::new();

    server.utilize(middleware! { |request|
        println!("logging request: {:?}", request.origin.uri);
    });


    let mut router = Nickel::router();

    router.get("/hello", middleware!("hello"));
    router.get("/ping", middleware! {
      let res = Res {
	res: "pong".to_string() 
      };
      res.to_json()
    }); 

    router.get("**", middleware!("Hello All"));
    
    server.utilize(router);
    server.listen("127.0.0.1:6767");
}
