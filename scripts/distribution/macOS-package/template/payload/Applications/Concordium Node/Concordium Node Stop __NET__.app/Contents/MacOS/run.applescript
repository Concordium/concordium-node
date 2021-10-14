
# Check whether a service is loaded.
to serviceIsLoaded(serviceName)
	# Use '$' so that "*node" does not also return "*node-collector".
	set checkStatusCmd to "launchctl list | grep " & serviceName & "$"
	try
		do shell script checkStatusCmd with administrator privileges
		return true
	on error number 1
		# grep returns '1' when the service isn't loaded, which results in an error being thrown.
		return false
	end try
end serviceIsLoaded

to stopService(serviceName)
	set stopServiceCmd to "launchctl remove " & serviceName
	do shell script stopServiceCmd with administrator privileges
end stopService

to main()
	set nodeService to "software.concordium.__NET__.node"
	set collectorService to "software.concordium.__NET__.node-collector"
	
	set nodeIsLoaded to serviceIsLoaded(nodeService)
	set collectorIsLoaded to serviceIsLoaded(collectorService)
	
	if (not nodeIsLoaded) and (not collectorIsLoaded) then
		display alert "Concordium __NET__ node is not running."
		return # exit
	end if
	
	if nodeIsLoaded then
		stopService(nodeService)
	end if
	if collectorIsLoaded then
		stopService(collectorService)
	end if
	
	display alert "Successfully stopped the Concordium __NET__ node."
	
end main

main()
