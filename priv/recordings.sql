CREATE DATABASE fs_recordings WITH OWNER fs_recordings,

CREATE TABLE recordings (
       id    	              uuid PRIMARY KEY,  	 
       created		      timestamp NOT NULL,          -- when this call was received
       cid_name      	      text DEFAULT '',             -- caller-id name
       cid_number	      text NOT NULL,               -- caller-id number
       destination_number     text NOT NULL,               -- recipients number
       file_path     	      text NOT NULL,               -- physical path to voicemail message on disk
       message_len   	      integer,                     -- length of recordings in seconds
       archived        	      boolean DEFAULT false            -- recording status
  );