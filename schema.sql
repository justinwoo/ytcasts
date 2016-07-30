CREATE TABLE downloads (link varchar(20) primary key unique, title varchar, created datetime);
CREATE INDEX i_downloads_link on downloads(link);
CREATE INDEX i_downloads_created on downloads(created);
