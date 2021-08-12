to startService(plistFile)
	set loadServiceCmd to "launchctl load '" & plistFile & "';"
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

to displayResultAlert(runCollector, prettyServiceName, nodeStatusCode, collectorStatusCode)
	set successMsg to "Successfully started the " & prettyServiceName & "."
	set failMsgPrefix to "Failed to start the " & prettyServiceName & ". Error: "
	if runCollector then
		if nodeStatusCode = "0" and collectorStatusCode = "0" then
			display alert successMsg
		else
			display alert failMsgPrefix & "(" & nodeStatusCode & "," & collectorStatusCode & ")"
		end if
	else
		if nodeStatusCode = "0" then
			display alert successMsg
		else
			display alert failMsgPrefix & nodeStatusCode
		end if
	end if
end displayResultAlert

to checkServiceStatus(serviceName)
	# Use $ to ensure that it is the end of a line. Otherwise '*node' will also return '*node-collector'.
	set terminatedServiceName to serviceName & "$"
	
	# 'cut -f2' gets the second field in a line, which is the statusCode.
	set getStatusCmd to "launchctl list | grep " & terminatedServiceName & " | cut -f2"
	
	return do shell script getStatusCmd with administrator privileges
end checkServiceStatus

to main()
	
	set runCollector to shouldRunCollector("/Library/Concordium Node/REPORT_TO_NETWORK_DASHBOARD___NET_UPPERCASE__")

	# Start services
	startService("/Library/Concordium Node/LaunchDaemons/software.concordium.__NET__.node.plist")
	if runCollector then
		startService("/Library/Concordium Node/LaunchDaemons/software.concordium.__NET__.node-collector.plist")
	end if
	
	# The node shows up as running briefly before exiting. Sleep x seconds to get reliable status result.
	delay 0.5
	
	# Get status codes
	set nodeStatus to checkServiceStatus("software.concordium.__NET__.node")
	set collectorStatus to "0" # default value, overriden if runCollector
	if runCollector then
		set collectorStatus to checkServiceStatus("software.concordium.__NET__.node-collector")
	end if
	
	displayResultAlert(runCollector, "Concordium __NET__ node", nodeStatus, collectorStatus)
	
end main

main()
