use rocket::fs::{FileServer, relative};

#[rocket::launch]
fn rocket() -> _ {
    rocket::build()
        .mount("/", FileServer::from(relative!("static/build")))
}
