use std::time::{SystemTime, Duration};
use std::collections::VecDeque;

#[derive(Clone)]
pub struct DataPoint {
    size: u64,
    time: u64
}

impl DataPoint {
    pub fn new(size: u64, time: u64) -> Self {
        DataPoint {
            size,
            time
        }
    }
}

pub struct StatsEngine {
    datapoints: VecDeque<DataPoint>,
    save_amount: u64,
}

impl StatsEngine {
    pub fn new(save_amount: u64) -> Self {
        StatsEngine {
            datapoints: VecDeque::new(),
            save_amount: save_amount,
        }
    }

    pub fn add_stat(&mut self, size: u64) {
        let _dur = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).expect("can't happen before epoch");

        //Check if we are over amount of stats to save
        if self.datapoints.len() >= self.save_amount as usize {
            //We're over capacity, pop oldest element
            self.datapoints.pop_front();
        }

        //We add the data point with the current amount of milliseconds since epoch.
        //Since Rust doesn't have a method to return millis in u64, we have to manually calculate that
        self.datapoints.push_back(DataPoint::new(size, (_dur.as_secs() * 1000) + (_dur.subsec_millis() as u64)));
    }

    pub fn clear(&mut self) {
        self.datapoints.clear();
    }

    pub fn calculate_total_tps_average(&self) -> f64 {
        //Get the first element and the last element in the queue.
        //We use their time fields to calculate total elapsed amount of time.
        let front_time = match self.datapoints.front() {
            Some(x) => x.time,
            None => 0
        };

        let back_time = match self.datapoints.back() {
            Some(x) => x.time,
            None => 0
        };

        let _dur = back_time-front_time;

        //Calculate the average amount of time used per transaction expressed in seconds.
        let avg_time = ((_dur as f64) / (self.datapoints.len() as f64)) / 1000 as f64;

        //Convert into transactions per second.
        avg_time.powi(-1)
    }

    pub fn calculate_last_five_min_tps_average(&self) -> f64 {
        let mut within_slot = VecDeque::new();
        let now = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).expect("can't happen before epoch");
        let minusfive = now.checked_sub(Duration::from_secs(300)).expect("less than 5 minutes spent");


        for point in self.datapoints.clone() {
            if Duration::from_millis(point.time) > minusfive {
                within_slot.push_back(point.clone());
            }
        }

        let front_time = match within_slot.front() {
            Some(x) => x.time,
            None => 0
        };

        let back_time = match within_slot.back() {
            Some(x) => x.time,
            None => 0
        };

        let _dur = back_time-front_time;

        //Calculate the average amount of time used per transaction expressed in seconds.
        let avg_time = ((_dur as f64) / (within_slot.len() as f64)) / 1000 as f64;

        //Convert into transactions per second.
        avg_time.powi(-1)
    }

    pub fn calculate_total_transferred_data_per_second(&self) -> f64 {
        //Get the first element and the last element in the queue.
        //We use their time fields to calculate total elapsed amount of time.
        let front_time = match self.datapoints.front() {
            Some(x) => x.time,
            None => 0
        };

        let back_time = match self.datapoints.back() {
            Some(x) => x.time,
            None => 0
        };

        let _dur = back_time-front_time;

        let mut total_size = 0;
        //Calculate total amount of data
        for i in 0..self.datapoints.len() {
            total_size += self.datapoints[i].size;
        }

        let calculated = ((_dur as f64) / (total_size as f64)) / 1000 as f64;

        calculated.powi(-1)
    }
}
