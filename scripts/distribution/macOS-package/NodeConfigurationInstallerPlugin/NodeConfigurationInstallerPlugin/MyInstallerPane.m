//
//  MyInstallerPane.m
//  NodeConfigurationInstallerPlugin
//
//  Created by Concordium on 30/07/2021.
//

#import "MyInstallerPane.h"

@implementation MyInstallerPane

- (NSString *)title
{
    return [[NSBundle bundleForClass:[self class]] localizedStringForKey:@"PaneTitle" value:nil table:nil];
}

// Invoked when either 'Go Back' or 'Continue' is pressed.
- (BOOL)shouldExitPane:(InstallerSectionDirection)aDir
{
    NSAlert *warning;

    // Check the direction of movement.
    if (aDir == InstallerDirectionForward) {
        
        if ([self nodeNamesAreValid]) {
            [self saveConfigurationToDisk];
        } else {
            // Create a warning dialog.
            warning = [[NSAlert alloc] init];
            if (warning != nil)
            {
                // Initialize the dialog.
                [warning addButtonWithTitle:@"OK"];
                [warning setMessageText:@"Invalid node names"];
                [warning setInformativeText:@"Node names cannot be empty or contain '\"'."];
                [warning setAlertStyle:NSAlertStyleInformational];
                
                // Display the warning dialog.
                [warning runModal];
            }
            // Prevent 'Continue' movement.
            return (NO);
        }
    }
    // Allow panel movement.
    return (YES);
}

- (void) saveConfigurationToDisk {
    
    NSString *const configFilePath = @"/tmp/software.concordium.node.install.config";
    
    // Create a file in /tmp.
    [[NSFileManager defaultManager] createFileAtPath:configFilePath contents:nil attributes:nil];
    
    // Get configuration from properties.
    // Checkbox states will be written as '1' or '0' for checked and unchecked, respectively.
    NSString *configData = [NSString stringWithFormat:@"CONCORDIUM_NODE_INSTALL_MAINNET_RUN_ON_STARTUP=%d\n"
                                                       "CONCORDIUM_NODE_INSTALL_MAINNET_RUN_AFTER_INSTALL=%d\n"
                                                       "CONCORDIUM_NODE_INSTALL_MAINNET_REPORT_TO_NETWORK_DASHBOARD=%d\n"
                                                       "CONCORDIUM_NODE_INSTALL_MAINNET_NODE_NAME=\"%@\"\n\n"
                                                       "CONCORDIUM_NODE_INSTALL_TESTNET_RUN_ON_STARTUP=%d\n"
                                                       "CONCORDIUM_NODE_INSTALL_TESTNET_RUN_AFTER_INSTALL=%d\n"
                                                       "CONCORDIUM_NODE_INSTALL_TESTNET_REPORT_TO_NETWORK_DASHBOARD=%d\n"
                                                       "CONCORDIUM_NODE_INSTALL_TESTNET_NODE_NAME=\"%@\"\n",
                            ([_oMainnetRunOnStartup state] == NSControlStateValueOn),
                            ([_oMainnetRunAfterInstall state] == NSControlStateValueOn),
                            ([_oMainnetReportToNetworkDashboard state] == NSControlStateValueOn),
                            [_oMainnetNodeName stringValue],
                            
                            ([_oTestnetRunOnStartup state] == NSControlStateValueOn),
                            ([_oTestnetRunAfterInstall state] == NSControlStateValueOn),
                            ([_oTestnetReportToNetworkDashboard state] == NSControlStateValueOn),
                            [_oTestnetNodeName stringValue]];
    
    // Write contents to file.
    [configData writeToFile:configFilePath atomically:YES encoding:NSUTF8StringEncoding error:nil];
}

// Checks that both the mainnet and testnet node names:
// - have length > 0
// - contain no double quotes
- (BOOL) nodeNamesAreValid
{
    NSString *mainnetName = [_oMainnetNodeName stringValue];
    NSString *testnetName = [_oTestnetNodeName stringValue];
    
    return      [mainnetName length] > 0
            &&  [testnetName length] > 0
            && ![mainnetName containsString:@"\""]
            && ![testnetName containsString:@"\""];
}

@end



