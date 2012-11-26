-record(clusterConfig,{
		   zkip,
		   port
	   }).

-record(topoConfig,
		{
			id,
			spouts = [],
			bolts = [],
			conns = [],
			status = ready
		}).

-record(spoutConfig,
		{
			id,
			count = 1,
			module,
			to=[]
  		}).

-record(boltConfig,
		{
			id,
			module,
			count = 1,
			from = [],
			to = []
		}).

-record(connConfig,
		{
			from,
			to,
			grouping
		}).
