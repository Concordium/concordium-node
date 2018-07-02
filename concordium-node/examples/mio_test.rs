extern crate bytes;
extern crate mio;
extern crate bincode;

use std::io::{Read, Write};
use std::time::Duration;
use std::collections::HashMap;
use mio::{Token, Ready, Events, Poll, PollOpt, Evented};
use mio::tcp::{TcpListener, TcpStream};
use mio::timer::Timer;
use bytes::{Bytes, BytesMut, Buf, BufMut};
use bincode::{serialize, deserialize};
use std::error::Error;
use std::io::prelude::*;
use std::fs::File;
use std::path::Path;

struct Connection {
    handle: TcpStream,
    currently_read: u64,
    expected_size: u64,
    buffer: Option<BytesMut>
}

impl Connection {
    fn new(handle: TcpStream) -> Self {
        Connection {
            handle: handle,
            currently_read: 0,
            expected_size: 0,
            buffer: None
        }
    }

    pub fn append_buffer(&mut self, new_data: &[u8] ) {
        if let Some(ref mut buf) = self.buffer {
            buf.reserve(new_data.len());
            buf.put_slice(new_data);
            self.currently_read += new_data.len() as u64;
        }
    }

    pub fn clear_buffer(&mut self) {
        if let Some(ref mut buf) = self.buffer {
            buf.clear();
        }
        self.buffer = None;
    }

    pub fn setup_buffer(&mut self) {
        self.buffer = Some(BytesMut::with_capacity(1024));
    }
}

const SERVER: Token = Token(0);
const TIMER_TOKEN: Token = Token(1);

const ADDR: &'static str = "127.0.0.1:8888";

pub fn main() {
    let addr = ADDR.parse().unwrap();
    let server = TcpListener::bind(&addr).unwrap();
    let poll = Poll::new().unwrap();

    // create a buffer to read into

    let mut count = 1;
    let mut connections = HashMap::new();

    poll.register(&server, SERVER, Ready::readable(), PollOpt::edge()).unwrap();

    // Create storage for events
    let mut events = Events::with_capacity(1024);

    let mut timer = Timer::default();
    poll.register(&timer, TIMER_TOKEN, Ready::readable(), PollOpt::edge())
        .unwrap();
    let flush_timeout = Duration::from_millis(5000);
    timer.set_timeout(flush_timeout, String::from("hello from 1s ago")).ok();

    let size_of_33554432:u64 = 33554432;
    let path = Path::new("/tmp/size-bin.dat");
    let mut file = match File::create(&path) {
        Err(why) => panic!("Couldn't create {}", why.description()),
        Ok(file) => file
    };
    file.write_all( &serialize(&size_of_33554432).unwrap());

    println!("Size of 33554432 in bincode is {:?}", serialize(&size_of_33554432));

    let mut files_count = 0;

    loop {
        poll.poll(&mut events, None).unwrap();

        for event in events.iter() {
            match event.token() {
                SERVER => {
                    let (stream, client_addr) = match server.accept() {
                        Ok((stream, client_addr)) => (stream, client_addr),
                        Err(e) => panic!("got an error when accepting a connection: {}", e),
                    };

                    println!("connection from: {}", client_addr);
                    count += 1;

                    poll.register(&stream, Token(count), Ready::readable(), PollOpt::level())
                        .unwrap();

                    let conn = Connection::new(stream);

                    connections.insert(count, conn);

                }
                TIMER_TOKEN => {
                    println!("number of open connections: {}", connections.len());
                    timer.set_timeout(flush_timeout, String::from("hello from 1s ago")).ok();
                }
                Token(c) => {
                    {
                        let mut conn = connections.get_mut(&c).unwrap();
                        if conn.expected_size > 0 && conn.currently_read == conn.expected_size {
                            println!("Completed file with {} size", conn.currently_read);
                            conn.expected_size = 0;
                            conn.currently_read = 0;
                            conn.clear_buffer();
                        } else if conn.expected_size > 0  {
                            let remainer = conn.expected_size-conn.currently_read;
                            if remainer < 512 {
                                let mut buf = vec![0u8; remainer as usize];
                                match conn.handle.read(&mut buf) {
                                    Ok(_) => {
                                        conn.append_buffer(&buf);
                                    }
                                    _ => {}
                                }
                            } else {
                                let mut buf = [0;512];
                                match conn.handle.read(&mut buf) {
                                    Ok(_) => {
                                        conn.append_buffer(&buf);
                                    }
                                    _ => {}
                                }
                            }
                            if conn.currently_read == conn.expected_size  {
                                println!("Completed file with {} size", conn.currently_read);
                                conn.expected_size = 0;
                                conn.currently_read = 0;
                                conn.clear_buffer();
                            }
                        } else {
                            let mut buf = [0;8];
                            match conn.handle.peek(&mut buf) {
                                Ok(n) => {
                                    if n == 8 {
                                        conn.handle.read_exact(&mut buf).unwrap();
                                        conn.expected_size = deserialize(&buf[..]).unwrap();
                                        conn.setup_buffer();
                                        println!("Starting new file, with expected size: {}", conn.expected_size);
                                    }
                                }
                                _ => println!("Error getting size..!")
                            }
                        }
                    }
                } 
            }
        }
    }
}