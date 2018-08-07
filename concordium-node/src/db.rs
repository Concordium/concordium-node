use rusqlite::{Connection, Error, Result};
use std::path::{Path};
use common;
use common::P2PNodeId;

pub struct P2PPeer {
    ip: String,
    port: u16,
    id: String,
}

impl P2PPeer {
    pub fn new(id: String, ip: String, port: u16) -> Self {
        P2PPeer {
            id,
            ip,
            port,
        }
    }

    pub fn to_P2P_Peer(self) -> common::P2PPeer {
        common::P2PPeer::from(P2PNodeId::from_string(self.id), self.ip.parse().unwrap(), self.port )
    }
}

pub struct P2PDB {
    conn: Option<Connection>,
}

impl P2PDB {
    pub fn new(path: &Path) -> Self {
        P2PDB {
            conn: match Connection::open(path) {
                Ok(x) => {
                    info!("Database loaded!");
                    Some(x)
                },
                Err(e) => {
                    error!("Couldn't open database! {:?}", e);
                    None
                },
            },
        }
    }

    pub fn get_banlist(&mut self) -> Option<Vec<P2PPeer>> {
        let mut list = vec![];
        match self.conn {
            Some(ref mut conn) => {
                match conn.prepare("SELECT id, ip, port FROM bans") {
                    Ok(mut x) => {
                        match x.query_map(&[], |row| {
                            P2PPeer {
                                id: row.get(0),
                                ip: row.get(1),
                                port: row.get(2),
                            }
                        }) {
                            Ok(rows) => {
                                for row in rows {
                                    match row {
                                        Ok(x) => {
                                            list.push(x);
                                        },
                                        Err(e) => {
                                            error!("Couldn't get item, {:?}", e);
                                        }
                                    }
                                }

                                Some(list)
                            },
                            Err(e) => {
                                error!("Couldn't map rows, {:?}", e);
                                None
                            },
                        }
                    },
                    Err(e) => {
                        error!("Couldn't execute query! {:?}", e);
                        None
                    }
                }
            },
            None => {
                None
            }
        }
    }

    pub fn create_banlist(&mut self) {
        match self.conn {
            Some(ref mut conn) => {
                match conn.execute("CREATE TABLE bans(id VARCHAR, ip VARCHAR, port INTEGER)", &[]) {
                    Ok(mut x) => {
                        
                    },
                    Err(e) => {
                        error!("Couldn't execute query! {:?}", e);
                        
                    }
                }
            },
            None => {
            }
        }
    }

    pub fn insert_ban(&mut self, id: String, ip: String, port: u16) -> bool{
        match self.conn {
            Some(ref mut conn) => {
                match conn.execute("INSERT INTO bans(id,ip,port) VALUES (?, ?, ?)", &[&id, &ip, &port]) {
                    Ok(updated) => {
                        if updated > 0 {
                            true
                        } else {
                            false
                        }
                    },
                    Err(e) => {
                        error!("Couldn't execute query! {:?}", e);
                        false
                    }
                }
            },
            None => {
                false
            }
        }
    }

    pub fn delete_ban(&mut self, id: String, ip: String, port: u16) -> bool{
        match self.conn {
            Some(ref mut conn) => {
                match conn.execute("DELETE FROM bans WHERE id = ? AND ip = ? AND port = ?", &[&id, &ip, &port]) {
                    Ok(updated) => {
                        if updated > 0 {
                            true
                        } else {
                            false
                        }
                    },
                    Err(e) => {
                        error!("Couldn't execute query! {:?}", e);
                        false
                    }
                }
            },
            None => {
                false
            }
        }
    }
}