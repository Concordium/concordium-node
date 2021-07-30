//
//  MyInstallerPane.h
//  NodeConfigurationInstallerPlugin
//
//  Created by Concordium on 30/07/2021.
//

#import <InstallerPlugins/InstallerPlugins.h>

@interface MyInstallerPane : InstallerPane

@property IBOutlet NSTextField *oNodeNameMainnet;
@property IBOutlet NSTextField *oNodeNameTestnet;

- (IBAction) validateNodeNames:(id)aSnd;

@end
