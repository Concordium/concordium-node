use serde::{de, Deserialize, Deserializer, Serialize};

tonic::include_proto!("concordium");

impl Serialize for node_info_response::IsInBakingCommittee {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer, {
        use node_info_response::IsInBakingCommittee::*;
        match self {
            ActiveInCommittee => serializer.serialize_str("ActiveInCommittee"),
            AddedButNotActiveInCommittee => {
                serializer.serialize_str("AddedButNotActiveInCommittee")
            }
            AddedButWrongKeys => serializer.serialize_str("AddedButWrongKeys"),
            NotInCommittee => serializer.serialize_str("NotInCommittee"),
        }
    }
}

impl<'de> Deserialize<'de> for node_info_response::IsInBakingCommittee {
    fn deserialize<D>(
        deserializer: D,
    ) -> Result<node_info_response::IsInBakingCommittee, D::Error>
    where
        D: Deserializer<'de>, {
        use node_info_response::IsInBakingCommittee::*;
        let str: String = Deserialize::deserialize(deserializer)?;
        let res = match str.as_str() {
            "ActiveInCommittee" => ActiveInCommittee,
            "AddedButNotActiveInCommittee" => AddedButNotActiveInCommittee,
            "AddedButWrongKeys" => AddedButWrongKeys,
            "NotInCommittee" => NotInCommittee,
            s => {
                return Err(de::Error::custom(format!(
                    "Failed deserializing string '{}' as IsInBakingCommittee",
                    s
                )))
            }
        };
        Ok(res)
    }
}
