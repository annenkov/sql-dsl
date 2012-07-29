--
-- PostgreSQL database dump
--

-- Started on 2012-02-08 23:31:15
SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 1465 (class 1259 OID 23754)
-- Dependencies: 3
-- Name: user_table; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE user_table (
    id integer NOT NULL,
    name character varying
);


--
-- TOC entry 1466 (class 1259 OID 23757)
-- Dependencies: 3 1465
-- Name: user_table_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE user_table_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- TOC entry 1743 (class 0 OID 0)
-- Dependencies: 1466
-- Name: user_table_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE user_table_id_seq OWNED BY user_table.id;


--
-- TOC entry 1744 (class 0 OID 0)
-- Dependencies: 1466
-- Name: user_table_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('user_table_id_seq', 2, true);


--
-- TOC entry 1733 (class 2604 OID 23759)
-- Dependencies: 1466 1465
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE user_table ALTER COLUMN id SET DEFAULT nextval('user_table_id_seq'::regclass);


--
-- TOC entry 1736 (class 0 OID 23754)
-- Dependencies: 1465
-- Data for Name: user_table; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO user_table (id, name) VALUES (1, 'John Doe');
INSERT INTO user_table (id, name) VALUES (2, 'Jane Doe');


--
-- TOC entry 1735 (class 2606 OID 23768)
-- Dependencies: 1465 1465
-- Name: ID_PK; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_table
    ADD CONSTRAINT "ID_PK" PRIMARY KEY (id);


--
-- TOC entry 1741 (class 0 OID 0)
-- Dependencies: 3
-- Name: public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- TOC entry 1742 (class 0 OID 0)
-- Dependencies: 1465
-- Name: user_table; Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON TABLE user_table FROM PUBLIC;
REVOKE ALL ON TABLE user_table FROM postgres;
GRANT ALL ON TABLE user_table TO postgres;
GRANT ALL ON TABLE user_table TO PUBLIC;


-- Completed on 2012-02-08 23:31:16

--
-- PostgreSQL database dump complete
--

