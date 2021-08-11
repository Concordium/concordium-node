
# Check whether a service is loaded.
to serviceIsLoaded(serviceName)
	# Use '$' so that "*node" does not also return "*node-collector".
	set checkStatusCmd to "launchctl list | grep " & serviceName & "$"
	try
		# grep will return -1 and thus produce an error, if the service isn't loaded.
		do shell script checkStatusCmd with administrator privileges
		return true
	on error # when grep returns no results
		return false
	end try
end serviceIsLoaded

to stopService(serviceName)
	set stopServiceCmd to "launchctl remove " & serviceName
	
	# Wrap in try so stopping non-loaded services don't cause an error prompt.
	try
		do shell script stopServiceCmd with administrator privileges
	end try
end stopService

to main()
	set nodeService to "software.concordium.__NET__.node"
	set collectorService to "software.concordium.__NET__.node-collector"
	
	set nodeIsLoaded to serviceIsLoaded(nodeService)
	set collectorIsLoaded to serviceIsLoaded(collectorService)
	
	if (not nodeIsLoaded) and (not collectorIsLoaded) then
		display alert "Concordium __NET__ node is not running."
	else
		stopService(nodeService)
		stopService(collectorService)
		display alert "Successfully stopped the Concordium __NET__ node."
	end if
end main

main()