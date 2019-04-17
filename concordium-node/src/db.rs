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

    pub fn get_banlist(&self) -> Option<Vec<BannedNode>> {
        match self.conn {
            Some(ref conn) => {
                let conn = safe_lock!(conn).ok()?;
                conn.prepare("SELECT id, ip, port FROM bans_id")
                    .map_err(|e| error!("Couldn't execute query! {:?}", e))
                    .ok()
                    .and_then(|mut x| {
                        match x.query_map(&[] as &[&dyn ToSql], |row| {
                            let s1: String = row.get(0)?;
                            Ok(BannedNode::ById(P2PNodeId::from_str(&s1).unwrap()))
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
                    })
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
