CREATE SCHEMA comicspreviews
    AUTHORIZATION comicspreviews;

CREATE SEQUENCE comicspreviews.seq_t_catalog_id;

ALTER SEQUENCE comicspreviews.seq_t_catalog_id OWNER TO comicspreviews;

CREATE TABLE comicspreviews.t_catalog(
    id_t_catalog bigint DEFAULT nextval('comicspreviews.seq_t_catalog_id'),
    date_creation date NOT NULL CONSTRAINT uq_t_catalog_date_creation UNIQUE,
    filepath VARCHAR(1024) NOT NULL,
    PRIMARY KEY (id_t_catalog)
)
WITH (
    OIDS = FALSE
);


ALTER TABLE comicspreviews.t_catalog OWNER to comicspreviews;

CREATE SEQUENCE comicspreviews.seq_t_comic_id;

ALTER SEQUENCE comicspreviews.seq_t_comic_id OWNER TO comicspreviews;

CREATE TABLE comicspreviews.t_comic (
    id_t_comic bigint DEFAULT nextval('comicspreviews.seq_t_comic_id'),
	id_t_catalog bigint,
    reference VARCHAR(256) NOT NULL CONSTRAINT uq_t_comic_reference UNIQUE,
    title VARCHAR(1024) NOT NULL,
    price VARCHAR(24),
    editor VARCHAR(256) NOT NULL,
    PRIMARY KEY (id_t_comic),
	CONSTRAINT fk_t_comic_t_catalog_id_t_catalog FOREIGN KEY (id_t_catalog) REFERENCES comicspreviews.t_catalog (id_t_catalog)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE comicspreviews.t_comic OWNER to comicspreviews;

CREATE INDEX ix_t_comic_id_t_catalog ON comicspreviews.t_comic (id_t_catalog);