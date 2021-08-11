# Prompt user
to promptUninstallOption()
	try
		set iconFile to (POSIX file getConcordiumIconPath() as alias)
		display dialog "Do you really want to uninstall Concordium Node from your system?
		
If you want to also delete the data, click 'Yes, and delete data'." buttons {"Yes, and delete data", "No", "Yes"} default button "Yes" cancel button "No" with title "Delete Concordium Node?" with icon iconFile
	on error
		# 'display dialog' returns an error and exits when 'No' (Cancel action) is pressed. We catch it and return a proper value instead.
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
		launchctl remove software.concordium.mainnet.node
		launchctl remove software.concordium.mainnet.node-collector
		launchctl remove software.concordium.testnet.node
		launchctl remove software.concordium.testnet.node-collector	
	" with administrator privileges
	end try
end removeServices


# Remove the installed files except for data.
to removeNodeFiles()
	# Wrap in try to avoid an error prompt when one or more files do not exist.
	try
		do shell script "
		rm -r '/Library/Concordium Node';
		rm -r '/Applications/Concordium Node';
		rm '/usr/local/bin/concordium-node';
		rm '/usr/local/bin/concordium-node-collector';
		rm '/Library/LaunchDaemons/software.concordium.mainnet.node.plist';
		rm '/Library/LaunchDaemons/software.concordium.mainnet.node-collector.plist';
		rm '/Library/LaunchDaemons/software.concordium.testnet.node.plist';
		rm '/Library/LaunchDaemons/software.concordium.testnet.node-collector.plist';				
		" with administrator privileges
	end try
end removeNodeFiles


# Remove the data files.
to removeDataFiles()
	# Wrap in try to avoid an error prompt when one or more files do not exist.
	try
		do shell script "
		rm -r '/Library/Application Support/Concordium Node'
		" with administrator privileges
	end try
end removeDataFiles


to main()
	
	set uninstall_option to promptUninstallOption()
	
	if uninstall_option = {button returned:"Yes"} then
		
		removeServices()
		removeNodeFiles()
		
	else if uninstall_option = {button returned:"Yes, and delete data"} then
		
		removeServices()
		removeNodeFiles()
		removeDataFiles()
		
	else if uninstall_option = {button returned:"No"} then
		
		# Do nothing
		
	end if
end main

main()
