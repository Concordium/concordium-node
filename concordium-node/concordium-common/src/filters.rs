use failure::Error;
use std::cmp::Ordering;

use crate::functor::AFuncCW;

/// Result of applying a filter
#[derive(PartialEq, Eq)]
pub enum FilterResult {
    Pass,
    Abort,
}

pub type FilterAFunc<T> = AFuncCW<T, FilterResult>;

/// Filtering function
pub struct Filter<T: Send> {
    func:     FilterAFunc<T>,
    priority: u8,
}

impl<T: Send> Eq for Filter<T> {}

// Always different functions, no point in comparing
impl<T: Send> PartialEq for Filter<T> {
    fn eq(&self, _: &Filter<T>) -> bool { false }
}

impl<T: Send> Ord for Filter<T> {
    fn cmp(&self, other: &Self) -> Ordering { self.priority.cmp(&other.priority) }
}

impl<T: Send> PartialOrd for Filter<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

impl<T: Send> Clone for Filter<T> {
    fn clone(&self) -> Self {
        Filter {
            func:     self.func.clone(),
            priority: self.priority,
        }
    }
}

/// Functor-like structure that applies filters sequentially in decreasing
/// priority
///
/// In case any of the filters fails, the whole execution will fail
#[derive(Default)]
pub struct Filters<T: Send> {
    filters: Vec<Filter<T>>,
}

impl<T: Send> Filters<T> {
    pub fn new() -> Self {
        Self {
            filters: Vec::new(),
        }
    }

    pub fn add_filter(&mut self, filter: FilterAFunc<T>, priority: u8) -> &mut Self {
        self.filters.push(Filter {
            func: filter,
            priority,
        });
        self
    }

    pub fn push_filter(&mut self, filter: Filter<T>) -> &mut Self {
        self.filters.push(filter);
        self
    }

    pub fn get_filters(&self) -> &[Filter<T>] { &self.filters }

    pub fn run_filters(&mut self, message: &T) -> Result<FilterResult, Error> {
        self.filters.sort();
        for cb in self.filters.iter().rev() {
            let res = (cb.func)(message)?;

            if FilterResult::Abort == res {
                return Ok(res);
            }
        }
        Ok(FilterResult::Pass)
    }
}
