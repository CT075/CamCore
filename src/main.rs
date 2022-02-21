use genawaiter::rc::{Co, Gen};

fn test1() -> Gen<i32, (), impl std::future::Future<Output = ()>> {
    Gen::new(|co| async move {
        test2(&co).await;
    })
}

async fn test2(co: &Co<i32, ()>) {
    let mut i = 1;
    while i < 6 {
        co.yield_(i).await;
        i += 1;
    }
}

fn main() {
    print!("hello, world!");
}
