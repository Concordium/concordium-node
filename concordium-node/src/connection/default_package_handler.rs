
pub struct DefaultRequestHandler {

}

impl MessageRequestHandler for DefaultRequestHandler {
    fn on_ping( & peer:P2PPeer ) -> Result<()>;
    fn on_find_node( & peer:P2PPeer, node_id: P2PNodeId ) -> Result<()>;
}

pub struct DefaultMessageHandler<R: DefaultRequestHandler> {
   request_handler: R; 
}

impl DefaultMessageHandler {
    fn new(&self, 
           request_handler: impl MessageRequestHandler) -> Self {
        DefaultMessageHandler {
            request_handler: request_handler
        }
    }
}

impl PackageHandler for DefaultPackageHandler {
    fn on_network_request( &self, &nr: NetworkRequest) -> Result<()> {
        Ok(())
    }

    fn on_network_response( &self, &nr: NetworkResponse) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    #[test]
    pub fn test_default_message_handler() {
        DefaultMessageHandler {
        }
    }
}
