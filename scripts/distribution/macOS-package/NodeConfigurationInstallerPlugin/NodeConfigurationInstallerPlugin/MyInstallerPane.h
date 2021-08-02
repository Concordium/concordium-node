//
//  MyInstallerPane.h
//  NodeConfigurationInstallerPlugin
//
//  Created by Concordium on 30/07/2021.
//

#import <InstallerPlugins/InstallerPlugins.h>

@interface MyInstallerPane : InstallerPane

@property IBOutlet NSButton *oMainnetRunOnStartup;
@property IBOutlet NSButton *oMainnetRunAfterInstall;
@property IBOutlet NSButton *oMainnetReportToNetworkDashboard;
@property IBOutlet NSTextField *oMainnetNodeName;

@property IBOutlet NSButton *oTestnetRunOnStartup;
@property IBOutlet NSButton *oTestnetRunAfterInstall;
@property IBOutlet NSButton *oTestnetReportToNetworkDashboard;
@property IBOutlet NSTextField *oTestnetNodeName;

@end
