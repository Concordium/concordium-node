use std::fmt::Debug;
use std::fs::File;
use std::io::Write;

pub struct OutputFiles {
    state_graph_file: File,
    state_data_file: File,
}

pub fn open_output_files(
    state_graph_file_path: &str,
    state_data_file_path: &str,
) -> OutputFiles {
    let state_graph_file = File::options()
        .append(true)
        .open(&state_graph_file_path)
        .expect(&format!("open state graph file: {}", state_graph_file_path));
    let state_data_file = File::options()
        .append(true)
        .open(&state_data_file_path)
        .expect(&format!("open state data file: {}", state_data_file_path));

    OutputFiles {
        state_graph_file,
        state_data_file,
    }
}

pub fn build_state_data(output: &mut OutputFiles, data: impl Debug) {
    writeln!(&mut output.state_data_file, "{:#?}", data).expect("write data data");
    writeln!(&mut output.state_data_file, "").expect("write data data");
}
