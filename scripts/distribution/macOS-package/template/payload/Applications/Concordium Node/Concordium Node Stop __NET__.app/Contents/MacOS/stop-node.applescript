
to serviceIsLoaded(serviceName)
	set checkStatusCmd to "sudo launchctl list | grep " & serviceName & "$"
	try
		do shell script checkStatusCmd with administrator privileges
		return true
	on error # when grep returns no results
		return false
	end try
end serviceIsLoaded

to stopService(serviceName)
	set stopServiceCmd to "sudo launchctl remove " & serviceName
	try
		do shell script stopServiceCmd with administrator privileges
	end try
end stopService

to main()
	set nodeService to "software.concordium.mainnet.node"
	set collectorService to "software.concordium.mainnet.node-collector"
	
	set nodeIsLoaded to serviceIsLoaded(nodeService)
	set collectorIsLoaded to serviceIsLoaded(collectorService)
	
	if (not nodeIsLoaded) and (not collectorIsLoaded) then
		display alert "Concordium mainnet node is not running."
	else
		stopService(nodeService)
		stopService(collectorService)
		display alert "Successfully stopped the Concordium mainnet node"
	end if
end main

main()