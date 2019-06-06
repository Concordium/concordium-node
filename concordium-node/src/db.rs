use crate::{common::P2PNodeId, p2p::banned_nodes::BannedNode};
use rusqlite::{types::ToSql, Connection};
use std::{
    path::Path,
    str::FromStr,
    sync::{Arc, Mutex},
};

#[derive(Clone)]
pub struct P2PDB {
    conn: Option<Arc<Mutex<Connection>>>,
}

impl Default for P2PDB {
    fn default() -> Self { P2PDB { conn: None } }
}

const RUSQLITE_NOTADATABASE_ERROR: i32 = 26;

fn open_banlist(path: &Path) -> Option<Arc<Mutex<Connection>>> {
    match Connection::open(path) {
        Ok(x) => {
            info!("Database loaded: {:?}", path);
            Some(Arc::new(Mutex::new(x)))
        }
        Err(e) => {
            error!("Couldn't open database! {:?}", e);
            None
        }
    }
}

impl P2PDB {
    pub fn new(path: &Path) -> Self {
        if path.exists() {
            P2PDB {
                conn: open_banlist(path),
            }
        } else {
            P2PDB { conn: None }
        }
    }

    pub fn get_banlist(&self) -> Option<Vec<BannedNode>> {
        match self.conn {
            Some(ref conn) => {
                let conn = safe_lock!(conn).ok()?;
                let by_id = conn
                    .prepare("SELECT id FROM bans_id")
                    .map_err(|e| {
                        if let rusqlite::Error::SqliteFailure(x, _) = e {
                            if x.extended_code == RUSQLITE_NOTADATABASE_ERROR {
                                panic!("Malformed database! Fix it or remove the file.");
                            } else {
                                error!("Couldn't execute query! {:?}", e)
                            }
                        } else {
                            panic!("Received a non SqliteFailure error from a db query: {}", e);
                        }
                    })
                    .ok()
                    .and_then(|mut x| {
                        match x.query_map(&[] as &[&dyn ToSql], |row| {
                            let s1: String = row.get(0)?;
                            if let Ok(id) = P2PNodeId::from_str(&s1) {
                                Ok(BannedNode::ById(id))
                            } else {
                                Err(rusqlite::Error::InvalidColumnType(
                                    s1.len(),
                                    rusqlite::types::Type::Text,
                                ))
                            }
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
                    });
                let by_addr = conn
                    .prepare("SELECT addr FROM bans_addr")
                    .map_err(|e| error!("Couldn't execute query! {:?}", e))
                    .ok()
                    .and_then(|mut x| {
                        match x.query_map(&[] as &[&dyn ToSql], |row| {
                            let s1: String = row.get(0)?;
                            if let Ok(addr) = s1.parse() {
                                Ok(BannedNode::ByAddr(addr))
                            } else {
                                Err(rusqlite::Error::InvalidColumnType(
                                    s1.len(),
                                    rusqlite::types::Type::Text,
                                ))
                            }
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
                    });
                Some(
                    by_id
                        .into_iter()
                        .chain(by_addr.into_iter())
                        .flat_map(std::iter::IntoIterator::into_iter)
                        .collect::<Vec<BannedNode>>(),
                )
            }
            None => None,
        }
    }

    pub fn create_banlist(mut self, path: &Path) -> Self {
        self.conn = open_banlist(path);
        if let Some(ref conn) = self.conn {
            match safe_lock!(conn) {
                Err(e) => {
                    error!("Couldn't lock connection: {:?}", e);
                }
                Ok(conn_mut) => {
                    if let Err(e) =
                        conn_mut.execute("CREATE TABLE bans_id(id VARCHAR)", &[] as &[&dyn ToSql])
                    {
                        error!("Couldn't execute query! {:?}", e)
                    };
                    if let Err(e) = conn_mut.execute("CREATE TABLE bans_addr(addr VARCHAR)", &[]
                        as &[&dyn ToSql])
                    {
                        error!("Couldn't execute query! {:?}", e)
                    };
                }
            }
        }
        self
    }

    pub fn insert_ban_id(&self, id: &str) -> bool {
        match self.conn {
            Some(ref conn) => match safe_lock!(conn) {
                Err(e) => {
                    error!("Couldn't lock connection: {:?}", e);
                    false
                }
                Ok(conn_mut) => {
                    if id == "" {
                        false
                    } else {
                        match conn_mut
                            .execute("INSERT INTO bans_id(id) VALUES (?)", &[&id as &dyn ToSql])
                        {
                            Ok(updated) => updated > 0,
                            Err(e) => {
                                error!("Couldn't execute query! {:?}", e);
                                false
                            }
                        }
                    }
                }
            },
            None => false,
        }
    }

    pub fn insert_ban_addr(&self, ip: &str) -> bool {
        match self.conn {
            Some(ref conn) => match safe_lock!(conn) {
                Err(e) => {
                    error!("Couldn't lock connection: {:?}", e);
                    false
                }
                Ok(conn_mut) => {
                    if ip == "" {
                        false
                    } else {
                        match conn_mut.execute("INSERT INTO bans_addr(addr) VALUES (?)", &[
                            &ip as &dyn ToSql
                        ]) {
                            Ok(updated) => updated > 0,
                            Err(e) => {
                                error!("Couldn't execute query! {:?}", e);
                                false
                            }
                        }
                    }
                }
            },
            None => false,
        }
    }

    pub fn delete_ban_id(&self, id: &str) -> bool {
        match self.conn {
            Some(ref conn) => match safe_lock!(conn) {
                Err(e) => {
                    error!("Couldn't lock connection: {:?}", e);
                    false
                }
                Ok(conn_mut) => {
                    if id == "" {
                        false
                    } else {
                        match conn_mut
                            .execute("DELETE FROM bans_id WHERE id = ?", &[&id as &dyn ToSql])
                        {
                            Ok(updated) => updated > 0,
                            Err(e) => {
                                error!("Couldn't execute query! {:?}", e);
                                false
                            }
                        }
                    }
                }
            },
            None => false,
        }
    }

    pub fn delete_ban_addr(&self, ip: &str) -> bool {
        match self.conn {
            Some(ref conn) => match safe_lock!(conn) {
                Err(e) => {
                    error!("Couldn't lock connection: {:?}", e);
                    false
                }
                Ok(conn_mut) => {
                    if ip == "" {
                        false
                    } else {
                        match conn_mut
                            .execute("DELETE FROM bans_addr WHERE addr = ?", &[&ip as &dyn ToSql])
                        {
                            Ok(updated) => updated > 0,
                            Err(e) => {
                                error!("Couldn't execute query! {:?}", e);
                                false
                            }
                        }
                    }
                }
            },
            None => false,
        }
    }
}
