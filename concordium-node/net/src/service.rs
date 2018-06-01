use mio::*;
use mio::net::TcpListener;
use mio::net::TcpStream;

const SERVER: Token = Token(0);
const CLIENT: Token = Token(1);

fn main() {
    let addr = "127.0.0.1:8888".parse().unwrap();

    let server = TcpListener::bind(&addr).unwrap();

    let poll = Poll::new().unwrap();

    poll.register(&server, SERVER, Ready::readable(), PollOpt::edge()).unwrap();

    let sock = TcpStream::connect(&addr).unwrap();
    poll.register(&sock, CLIENT, Ready::readable(), PollOpt::edge()).unwrap();

    let mut events = Events::with_capacity(1024);

    loop {
        poll.poll(&mut events, None).unwrap();

        for event in events.iter() {
            match event.token() {
                SERVER => {
                    let socket = server.accept();

                    match socket {
                        Some(x) => {
                            match x.peer_addr() {
                                Ok(y) => println!("Peer addr: {}", y),
                                Err => println!("Couldn't retrieve peer address")
                            }
                        },
                        None => println!("Broken")
                    };
                },
                CLIENT => {
                    return;
                },
                _ => unreachable!(),
            };
        }
    }
}