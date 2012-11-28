-record(clusterConfig,{
		   zkip,
		   port
	   }).

-record(topoConfig,
		{
			id,
			spouts = [],
			bolts = [],
			conns = []
		}).

-record(spoutConfig,
		{
			id,
			topoId,
			count = 1,
			module,
			to=[]
  		}).

-record(boltConfig,
		{
			id,
			topoId,
			count = 1,
			module,
			from = [],
			to = []
		}).

-record(connConfig,
		{
			from,
			to,
			grouping
		}).
