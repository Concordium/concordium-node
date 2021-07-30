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

- (void)didEnterPane:(InstallerSectionDirection)aDir
{
    // Initialize the node name fields.
    [_oNodeNameMainnet setStringValue:@"mainnet node name.."];
    [_oNodeNameTestnet setStringValue:@"test node name.."];
    
    // Disable the 'Continue' button.
    [self setNextEnabled:false];
    
    // Enabled the 'Go Back' button.
    [self setPreviousEnabled:true];
}

// Invoked when either 'Go Back' or 'Continue' is pressed.
- (BOOL)shouldExitPane:(InstallerSectionDirection)aDir
{
    NSAlert *tWrn;
    
    // Check the direction of movement.
    if (aDir == InstallerDirectionForward) {
        
        // Check if either node name is empty.
        if (([[_oNodeNameMainnet stringValue] length] > 0) && ([[_oNodeNameTestnet stringValue] length] > 0)) {
            [self saveConfigurationToDisk];
        } else {
            // Create a warning dialog.
            tWrn = [[NSAlert alloc] init];
            if (tWrn != nil)
            {
                // Initialize the dialog.
                [tWrn addButtonWithTitle:@"OK"];
                [tWrn setMessageText:@"Node names cannot be empty"];
                [tWrn setInformativeText:@"Please enter valid node names."];
                [tWrn setAlertStyle:NSAlertStyleInformational];
                
                // Display the warning dialog.
                [tWrn runModal];
            }
            // Disable the Continue button.
            [self setNextEnabled:NO];
            
            // Prevent 'Continue' movement.
            return (NO);
        }
    }
    // Allow panel movement.
    return (YES);
}

- (void) saveConfigurationToDisk {
    
    // Create a file in /tmp.
    [[NSFileManager defaultManager] createFileAtPath:@"/tmp/concordium.node.install.config" contents:nil attributes:nil];
    
    NSString *configData = [NSString stringWithFormat:@"mainnet_node_name=%@\ntestnet_node_name=%@", [_oNodeNameMainnet stringValue], [_oNodeNameTestnet stringValue]];
    [configData writeToFile:@"/tmp/concordium.node.install.config" atomically:YES encoding:NSUTF8StringEncoding error:nil];
}

- (IBAction) validateNodeNames:(id)aSnd
{
    BOOL tChk;
    // Check the node name fields.
    tChk = ([[_oNodeNameMainnet stringValue] length] > 0) && ([[_oNodeNameTestnet stringValue] length] > 0);
    
    // Enable/disable the Continue button.
    [self setNextEnabled:tChk];
}

@end



