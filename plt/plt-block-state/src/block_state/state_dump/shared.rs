use crate::block_state::blob_store;
use concordium_base::hashes::Hash;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::Write;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Copy)]
pub struct NodeId(pub u64);

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct StateDumpContext {
    mutable: Arc<Mutex<StateDumpContextMutable>>,
}

impl StateDumpContext {
    pub fn new(next_node_id: NodeId) -> Self {
        Self {
            mutable: Arc::new(Mutex::new(StateDumpContextMutable::new(next_node_id))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StateDumpContextMutable {
    next_node_id: NodeId,
    blob_ref_to_node_id: HashMap<blob_store::Reference, (NodeId, Option<Hash>)>,
}

impl StateDumpContextMutable {
    pub fn new(next_node_id: NodeId) -> Self {
        Self {
            next_node_id,
            blob_ref_to_node_id: Default::default(),
        }
    }
}

pub struct StateDumpBuilder {
    files: OutputFiles,
    context: StateDumpContext,
}

pub struct OutputFiles {
    state_graph_file: File,
    state_data_file: File,
}

pub struct OutputFilesPaths {
    pub state_graph_file_path: String,
    pub state_data_file_path: String,
}

impl StateDumpBuilder {
    pub fn new(context: StateDumpContext, output_paths: OutputFilesPaths) -> Self {
        let files = open_output_files(&output_paths);

        Self { files, context }
    }

    pub fn build_state_data(
        &mut self,
        blob_ref: blob_store::Reference,
        hash: Hash,
        data: impl Debug,
    ) {
        writeln!(
            &mut self.files.state_data_file,
            "{}/{}:\n{:#?}",
            blob_ref, hash, data
        )
        .expect("write data data");
        writeln!(&mut self.files.state_data_file).expect("write data data");
    }

    pub fn build_blob_ref_node_no_edge(
        &mut self,
        label: &str,
        blob_ref: blob_store::Reference,
        hash: Option<Hash>,
    ) -> (NodeId, bool) {
        let mut nodes_guard = self.context.mutable.lock().unwrap();

        if let Some((existing_node_id, existing_hash)) =
            nodes_guard.blob_ref_to_node_id.get(&blob_ref)
        {
            if hash != *existing_hash {
                panic!(
                    "hash does not match for blob ref {}, existing: {:?}, new: {:?}",
                    blob_ref, existing_hash, hash
                );
            }
            (*existing_node_id, false)
        } else {
            let node_id = nodes_guard.next_node_id;
            nodes_guard.next_node_id.0 += 1;
            nodes_guard
                .blob_ref_to_node_id
                .insert(blob_ref, (node_id, hash));

            let node_label = if let Some(hash) = hash {
                format!("{}/{:?}", escape_quotes(label), hash)
            } else {
                escape_quotes(label)
            };
            write!(
                self.files.state_graph_file,
                "    {}  [label=\"{}\"]",
                node_id, node_label
            )
            .expect("write to graph data file");

            (node_id, true)
        }
    }

    pub fn build_comp_node_no_edge(&mut self, label: &str, hash: Option<Hash>) -> NodeId {
        let mut nodes_guard = self.context.mutable.lock().unwrap();
        let node_id = nodes_guard.next_node_id;
        nodes_guard.next_node_id.0 += 1;

        let node_label = if let Some(hash) = hash {
            format!("{}/{:?}", escape_quotes(label), hash)
        } else {
            escape_quotes(label)
        };
        write!(
            self.files.state_graph_file,
            "    {}  [label=\"{}\"]",
            node_id, node_label
        )
        .expect("write to graph data file");

        node_id
    }

    pub fn build_blob_ref_edge(
        &mut self,
        label: &str,
        source: NodeId,
        target: NodeId,
        blob_ref: blob_store::Reference,
    ) {
        let edge_label = format_args!("{}{}", label, blob_ref);
        write!(
            self.files.state_graph_file,
            "    {} -> {} [label=\"{}\"]",
            source, target, edge_label
        )
        .expect("write to graph data file");
    }

    pub fn build_comp_edge(&mut self, label: &str, source: NodeId, target: NodeId) {
        let edge_label = label;
        write!(
            self.files.state_graph_file,
            "    {} -> {} [arrowhead=\"none\" label=\"{}\"]",
            source, target, edge_label
        )
        .expect("write to graph data file");
    }

    pub fn build_blob_ref_node(
        &mut self,
        parent: NodeId,
        node_label: &str,
        edge_label: &str,
        blob_ref: blob_store::Reference,
        hash: Option<Hash>,
    ) -> Option<NodeId> {
        let (node_id, created) = self.build_blob_ref_node_no_edge(node_label, blob_ref, hash);
        self.build_blob_ref_edge(edge_label, parent, node_id, blob_ref);
        created.then_some(node_id)
    }

    pub fn build_comp_node(
        &mut self,
        parent: NodeId,
        node_label: &str,
        edge_label: &str,
        hash: Option<Hash>,
    ) -> NodeId {
        let node_id = self.build_comp_node_no_edge(node_label, hash);
        self.build_comp_edge(edge_label, parent, node_id);
        node_id
    }
}

fn open_output_files(output_paths: &OutputFilesPaths) -> OutputFiles {
    let state_graph_file = File::options()
        .append(true)
        .open(&output_paths.state_graph_file_path)
        .unwrap_or_else(|_| {
            panic!(
                "open state graph file: {}",
                output_paths.state_graph_file_path
            )
        });
    let state_data_file = File::options()
        .append(true)
        .open(&output_paths.state_data_file_path)
        .unwrap_or_else(|_| {
            panic!(
                "open state data file: {}",
                output_paths.state_data_file_path
            )
        });

    OutputFiles {
        state_graph_file,
        state_data_file,
    }
}

fn escape_quotes(str: &str) -> String {
    str.replace('"', "\\\"")
}
