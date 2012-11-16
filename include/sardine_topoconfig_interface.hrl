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
			module,
			to=[],
			count
  		}).

-record(boltConfig,
		{
			id,
			module,
			from = [],
			to = [],
			count = 1
		}).

-record(connConfig,
		{
			from,
			to,
			grouping
		}).
