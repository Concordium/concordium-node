# Prompt user
to promptUninstallOption()
	try
		set icon_file to (POSIX file getConcordiumIconPath() as alias)
		# If the user presses cancel, the script will terminate with error code -128, which is fine in this case.
		display dialog "Do you really want to uninstall Concordium Node from your system?
		
If you want to also delete the data, click 'Yes, and delete data'." buttons {"Yes, and delete data", "No", "Yes"} default button "Yes" cancel button "No" with title "Delete Concordium Node?" with icon icon_file
	on error
		return {button returned:"No"}
	end try
end promptUninstallOption

# Get icon path in Resources/ 
to getConcordiumIconPath()
	set scriptFolder to (POSIX path of ((path to me as text) & "::"))
	return scriptFolder & "../Resources/concordium_logo.icns"
end getConcordiumIconPath


# Removes all the concordium services.
to removeServices()
	# Wrap in try to ignore an error when no services were loaded.
	try
		do shell script "
		sudo launchctl remove software.concordium.mainnet.node
		sudo launchctl remove software.concordium.mainnet.node-collector
		sudo launchctl remove software.concordium.testnet.node
		sudo launchctl remove software.concordium.testnet.node-collector	
	" with administrator privileges
	end try
end removeServices

# Filter a list to only include items that exist on disk.
#	Input: A list of strings (in POSIX format).
#	Output: A list of file alias objects that exist on disk.
to filterItemsOnDisk(itemsToCheck)
	set itemsFoundOnDisk to {}
	repeat with x in itemsToCheck
		try
			# 'as alias' will fail if the file/folder does not exist.
			set foundOnDisk to POSIX file x as alias
			copy foundOnDisk to end of itemsFoundOnDisk
		end try
	end repeat
	return itemsFoundOnDisk
end filterItemsOnDisk

# Use Finder to delete an item or a list of items on disk.
# Will return an error if any item does _not_ exist on disk.
to deleteItems(itemsToDelete)
	tell application "Finder"
		delete itemsToDelete
	end tell
end deleteItems

to main()
	
	set appPath to {"/Library/Concordium Node"}
	set supportAppsPath to {"/Applications/Concordium Node"}
	set symlinkPaths to {"/usr/local/bin/concordium-node-__VERSION__", "/usr/local/bin/concordium-node-collector-__VERSION__"}
	set servicePaths to {"/Library/LaunchDaemons/software.concordium.mainnet.node.plist", "/Library/LaunchDaemons/software.concordium.mainnet.node-collector.plist", "/Library/LaunchDaemons/software.concordium.testnet.node.plist", "/Library/LaunchDaemons/software.concordium.testnet.node-collector.plist"}
	set itemsInstalled to appPath & supportAppsPath & symlinkPaths & servicePaths
	
	set dataFolder to {"/Library/Application Support/Concordium Node"}
	
	set uninstall_option to promptUninstallOption()
	
	if uninstall_option = {button returned:"Yes"} then
		
		set itemsToDelete to filterItemsOnDisk(itemsInstalled)
		removeServices()
		deleteItems(itemsToDelete)
		
		
	else if uninstall_option = {button returned:"Yes, and delete data"} then
		
		set itemsToDelete to filterItemsOnDisk(itemsInstalled & dataFolder)
		removeServices()
		deleteItems(itemsToDelete)
		
	else if uninstall_option = {button returned:"No"} then
		
		# Do nothing
		
	end if
end main

main()
