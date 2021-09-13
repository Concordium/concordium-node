//
//  MyInstallerPane.m
//  NodeConfigurationInstallerPlugin
//
//  Created by Concordium on 30/07/2021.
//

#import "MyInstallerPane.h"
#import <os/log.h>

@implementation MyInstallerPane

- (NSString *)title
{
    return [[NSBundle bundleForClass:[self class]] localizedStringForKey:@"PaneTitle" value:nil table:nil];
}

// Invoked when either 'Go Back' or 'Continue' is pressed.
- (BOOL)shouldExitPane:(InstallerSectionDirection)aDir
{
    os_log_t customLog = os_log_create("software.concordium.node.installer", "shouldExitPane");
    NSAlert *warning;

    // Check the direction of movement.
    if (aDir == InstallerDirectionForward) {
        
        os_log(customLog, "'Continue' was pressed");

        if ([self nodeNamesAreValid]) {
            os_log(customLog, "Node names valid: Save configuration to disk");
            [self saveConfigurationToDisk];
        } else {
            os_log(customLog, "Node names invalid: show alert");
            // Create a warning dialog.
            os_log(customLog, "NSAlert: alloc and init");
            warning = [[NSAlert alloc] init];
            if (warning != nil)
            {
                os_log(customLog, "NSAlert: alloc and init succeeded");
                // Initialize the dialog.
                [warning addButtonWithTitle:@"OK"];
                [warning setMessageText:@"Invalid node names"];
                [warning setInformativeText:@"Node names cannot be empty or contain '\"'."];
                [warning setAlertStyle:NSAlertStyleInformational];
                
                // Display the warning dialog.
                os_log(customLog, "Show NSAlert modal");
                [warning runModal];
            }
            // Prevent 'Continue' movement.
            os_log(customLog, "Prevent forward movement");
            return (NO);
        }
    }

    // Allow panel movement.
    os_log(customLog, "Allow panel movement");
    return (YES);
}

- (void) saveConfigurationToDisk {
    
    os_log_t customLog = os_log_create("software.concordium.node.installer", "saveConfigurationToDisk");
    NSString *const configFilePath = @"/tmp/software.concordium.node.install.config";
    
    // Create a file in /tmp.
    os_log(customLog, "Create file: %@", configFilePath);
    BOOL createdFile = [[NSFileManager defaultManager] createFileAtPath:configFilePath contents:nil attributes:nil];
    os_log(customLog, "File-creation result: %d", createdFile);
    
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
    os_log(customLog, "Write to file: %@", configFilePath);
    NSError *__autoreleasing  _Nullable * writeToFileError = nil;
    BOOL writeSucceeded = [configData writeToFile:configFilePath atomically:YES encoding:NSUTF8StringEncoding error:writeToFileError];
    
    if (writeSucceeded) {
        os_log(customLog, "Writing to file succeeded");
    } else {
        os_log(customLog, "Writing to file failed with error %ld: %@", [*writeToFileError code], [*writeToFileError localizedDescription]);
    }
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



