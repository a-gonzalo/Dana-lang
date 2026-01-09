use crate::runtime::evaluator::EvalError;
use thiserror::Error;

/// High-level runtime errors that can wrap module-specific errors
#[derive(Error, Debug, Clone)]
pub enum RuntimeError {
    #[error("Evaluator error: {0}")]
    Eval(#[from] EvalError),

    #[error("Scheduler error: {0}")]
    Scheduler(String),

    #[error("{0}")]
    Native(String),

    #[error("Build error: {0}")]
    Build(String),

    #[error("IO error: {0}")]
    Io(String),

    #[error("Other runtime error: {0}")]
    Other(String),
}
