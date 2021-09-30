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
                [warning setInformativeText:@"Node names must be between 1 and 100 characters in length and can only contain a-z, A-Z, 0-9, spaces, '-', or '_'. Additionally, names cannot start or end with spaces."];
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
// - have length > 0 and <= 100
// - only contain characters in [a-zA-Z0-9-_ ]
// - cannot start or end with spaces
- (BOOL) nodeNamesAreValid
{
    // NB: This character set must be accepted by the service file XML parser.
    // This naturally excludes '<', '>', etc., but the service won't start with a sequence of ',.' in the node name either.
    NSCharacterSet *allowedChars = [[NSCharacterSet characterSetWithCharactersInString:@"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890-_ "] invertedSet];

    // Default maximum node name length that the collector-backend will accept. Measured in UTF-8 encoded bytes.
    int maxLen = 100;

    NSString *mainnetName = [_oMainnetNodeName stringValue];
    NSString *testnetName = [_oTestnetNodeName stringValue];

    // NB: Get the actual length in bytes if allowedChars is changed to contain non-ascii characters.
    NSUInteger mainnetNameLen = [mainnetName length];
    NSUInteger testnetNameLen = [mainnetName length];

    return      mainnetNameLen > 0
            &&  mainnetNameLen <= maxLen
            &&  testnetNameLen > 0
            &&  testnetNameLen <= maxLen
            &&  ![mainnetName hasPrefix:@" "]
            &&  ![mainnetName hasSuffix:@" "]
            && !([mainnetName rangeOfCharacterFromSet:allowedChars].location != NSNotFound)
            && !([testnetName rangeOfCharacterFromSet:allowedChars].location != NSNotFound);
}

@end



