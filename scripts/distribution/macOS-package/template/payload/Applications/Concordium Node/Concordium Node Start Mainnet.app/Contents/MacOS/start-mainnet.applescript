to startService(plistFile)
	set loadServiceCmd to "sudo launchctl load '" & plistFile & "';"
	do shell script loadServiceCmd with administrator privileges
end startService

# Checks whether the runCollector file exists.
to shouldRunCollector(configFile)
	try
		# 'as alias' will throw an error, if the file does not exist
		(POSIX file configFile) as alias
		return true
	on error
		return false
	end try
end shouldRunCollector

# Converts the empty statusCode to "-". The empty status code occurs when the service isn't loaded.
to toErrorCode(statusCode)
	if statusCode = "" then
		return "-"
	else
		return statusCode
	end if
end toErrorCode

to displayResultAlert(runCollector, prettyServiceName, nodeStatusCode, collectorStatusCode)
	set successMsgPrefix to "Successfully started the " & prettyServiceName
	set failMsgPrefix to "Failed to start the " & prettyServiceName & ". Error: "
	if runCollector then
		if nodeStatusCode = "0" and collectorStatusCode = "0" then
			display alert successMsgPrefix & " and collector."
		else
			display alert failMsgPrefix & "(" & nodeStatusCode & "," & collectorStatusCode & ")"
		end if
	else
		if nodeStatusCode = "0" then
			display alert successMsgPrefix & "."
		else
			display alert failMsgPrefix & nodeStatusCode
		end if
	end if
end displayResultAlert

to checkServiceStatus(serviceName)
	# Use $ to ensure that it is the end of a line. Otherwise '*node' will also return '*node-collector'.
	set terminatedServiceName to serviceName & "$"
	
	# 'cut -f2' gets the second field in a line, which is the exitCode.
	set getStatusCmd to "sudo launchctl list | grep " & terminatedServiceName & " | cut -f2"
	
	return do shell script getStatusCmd with administrator privileges
end checkServiceStatus

to main()
	
	startService("/Library/Concordium Node/LaunchDaemons/software.concordium.mainnet.node.plist")
	set nodeStatus to checkServiceStatus("software.concordium.mainnet.node")
	
	set collectorStatus to "0" # default value, overriden if runCollector
	
	set runCollector to shouldRunCollector("/Library/Concordium Node/REPORT_TO_NETWORK_DASHBOARD_MAINNET")
	if runCollector then
		startService("/Library/Concordium Node/LaunchDaemons/software.concordium.mainnet.node-collector.plist")
		set collectorStatus to checkServiceStatus("software.concordium.mainnet.nodse-collector")
	end if
	
	displayResultAlert(runCollector, "Concordium mainnet node", nodeStatus, collectorStatus)
	
end main

main()