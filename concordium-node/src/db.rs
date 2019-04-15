use crate::common::{self, P2PNodeId, PeerType};
use rusqlite::{types::ToSql, Connection};
use std::{
    path::Path,
    sync::{Arc, Mutex},
};

pub struct P2PPeer {
    ip:   String,
    port: u16,
    id:   String,
}

impl P2PPeer {
    pub fn new(id: String, ip: String, port: u16) -> Self { P2PPeer { id, ip, port } }

    pub fn to_peer(self) -> common::P2PPeer {
        common::P2PPeer::from(
            PeerType::Node,
            P2PNodeId::from_str(&self.id).unwrap(),
            self.ip.parse().unwrap(),
            self.port,
        )
    }
}

#[derive(Clone)]
pub struct P2PDB {
    conn: Option<Arc<Mutex<Connection>>>,
}

impl Default for P2PDB {
    fn default() -> Self { P2PDB { conn: None } }
}

impl P2PDB {
    pub fn new(path: &Path) -> Self {
        P2PDB {
            conn: match Connection::open(path) {
                Ok(x) => {
                    info!("Database loaded!");
                    Some(Arc::new(Mutex::new(x)))
                }
                Err(e) => {
                    error!("Couldn't open database! {:?}", e);
                    None
                }
            },
        }
    }

    pub fn get_banlist(&self) -> Option<Vec<P2PPeer>> {
        match self.conn {
            Some(ref conn) => {
                let conn = safe_lock!(conn).ok()?;
                let res = conn.prepare("SELECT id, ip, port FROM bans");

                match res {
                    Ok(mut x) => {
                        match x.query_map(&[] as &[&dyn ToSql], |row| {
                            Ok(P2PPeer {
                                id:   row.get(0)?,
                                ip:   row.get(1)?,
                                port: row.get(2)?,
                            })
                        }) {
                            Ok(rows) => {
                                let mut list = vec![];
                                for row in rows {
                                    match row {
                                        Ok(x) => list.push(x),
                                        Err(e) => error!("Couldn't get item, {:?}", e),
                                    }
                                }

                                Some(list)
                            }
                            Err(e) => {
                                error!("Couldn't map rows, {:?}", e);
                                None
                            }
                        }
                    }
                    Err(e) => {
                        error!("Couldn't execute query! {:?}", e);
                        None
                    }
                }
            }
            None => None,
        }
    }

    pub fn create_banlist(&self) {
        if let Some(ref conn) = self.conn {
            match safe_lock!(conn) {
                Err(e) => {
                    error!("Couldn't lock connection: {:?}", e);
                }
                Ok(conn_mut) => {
                    if let Err(e) = conn_mut.execute(
                        "CREATE TABLE bans(id VARCHAR, ip VARCHAR, port INTEGER)",
                        &[] as &[&dyn ToSql],
                    ) {
                        error!("Couldn't execute query! {:?}", e);
                    }
                }
            }
        }
    }

    pub fn insert_ban(&self, id: &str, ip: &str, port: u16) -> bool {
        match self.conn {
            Some(ref conn) => match safe_lock!(conn) {
                Err(e) => {
                    error!("Couldn't lock connection: {:?}", e);
                    false
                }
                Ok(conn_mut) => {
                    match conn_mut.execute("INSERT INTO bans(id,ip,port) VALUES (?, ?, ?)", &[
                        &id,
                        &ip,
                        &port as &dyn ToSql,
                    ]) {
                        Ok(updated) => updated > 0,
                        Err(e) => {
                            error!("Couldn't execute query! {:?}", e);
                            false
                        }
                    }
                }
            },
            None => false,
        }
    }

    pub fn delete_ban(&self, id: String, ip: String, port: u16) -> bool {
        match self.conn {
            Some(ref conn) => match safe_lock!(conn) {
                Err(e) => {
                    error!("Couldn't lock connection: {:?}", e);
                    false
                }
                Ok(conn_mut) => {
                    match conn_mut.execute(
                        "DELETE FROM bans WHERE id = ? AND ip = ? AND port = ?",
                        &[&id, &ip, &port as &dyn ToSql],
                    ) {
                        Ok(updated) => updated > 0,
                        Err(e) => {
                            error!("Couldn't execute query! {:?}", e);
                            false
                        }
                    }
                }
            },
            None => false,
        }
    }
}
