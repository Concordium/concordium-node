use failure::Fallible;

pub trait Archive: Sized {
    type Ok;

    fn write_str<T: AsRef<str>>(&mut self, s: T) {
        self.write_u32::<NetworkEndian>
    }

    fn write_u16(&self,  );
        archive.write_u64( Utc::now().timestamp_millis() as u64);
        archive.write_u16( message_type);

    /*fn write<T>(&self, T o) -> Fallible<Ok>
        where T: {
    }*/
}

pub trait Serializable {
    fn serialize<A>(&self, archive: A) -> Fallible<A::Ok>
        where A: Archive;

    fn deserialize<A>(&mut self, archive: A) -> Fallible<A::Ok>
        where A: Archive;
}
