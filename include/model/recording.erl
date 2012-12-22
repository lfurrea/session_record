-module(recording, [Id::uuid(), 
		    Created::datetime(),
		    CidName::string(),
		    CidNumber::string(),
		    DestinationNumber::string(),
		    FilePath::string(),
		    MessageLen::integer(),
		    Archived::boolean()
		   ]).
%-has({orders, many}).
