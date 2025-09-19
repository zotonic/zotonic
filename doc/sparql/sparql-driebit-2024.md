# Onderzoek ZZC + NDE

# Onderzoek SPARQL + Postgres

Vraag: zien we mogelijkheden voor een hybride oplossing SPARQL + Postgres?

## Curated list of various semantic web and linked data resources.

[https://github.com/semantalytics/awesome-semantic-web](https://github.com/semantalytics/awesome-semantic-web)

## RDF stores

### OCAML-RDF

[https://www.good-eris.net/ocaml-rdf/](https://www.good-eris.net/ocaml-rdf/)

- Storage in PostgreSQL
- Sparql 1.1
- LGPLv3

### Elixir - A PostgreSQL-based RDF/SPARQL engine

- Paper [https://f1000research.com/assets/download/1117034](https://f1000research.com/assets/download/1117034)
- Paper [https://jcheminf.biomedcentral.com/articles/10.1186/s13321-021-00515-1](https://jcheminf.biomedcentral.com/articles/10.1186/s13321-021-00515-1)
- Websites [https://idsm.elixir-czech.cz/](https://idsm.elixir-czech.cz/)  [https://www.elixir-czech.cz](https://www.elixir-czech.cz/)
- Maps Sparql to SQL  (using a database schema to RDF mapping)
- Broncode? [https://github.com/idsm-src/chemweb/tree/develop](https://github.com/idsm-src/chemweb/tree/develop) of [https://ip-147-251-124-124.flt.cloud.muni.cz/chemdb/chemweb](https://ip-147-251-124-124.flt.cloud.muni.cz/chemdb/chemweb)

### Apache JENA

- [https://jena.apache.org](https://jena.apache.org/)
- Free open source Java framework for building Semantic Web and Linked Data applications
- Ik heb referenties gezien dat de data ook in PostgreSQL kan worden opgeslagen
- Sparql 1.1

### Apache AGE - Graph Database for PostgreSQL

- [https://age.apache.org](https://age.apache.org/)
- GraphQL API
- [https://github.com/kracr/sparql-cypher-transpiler](https://github.com/kracr/sparql-cypher-transpiler) voor mappen van SPARQL queries
- [https://github.com/wheeler89/sparql-to-agecypher](https://github.com/wheeler89/sparql-to-agecypher) A SPARQL to Apache AGE-openCypher transpiler for query interoperability between RDF and Property Graph databases

## Misc

### RDF on Elixir (the programming language)

- [https://rdf-elixir.dev](https://rdf-elixir.dev/)
- [https://github.com/rdf-elixir](https://github.com/rdf-elixir)
- [https://pragprog.com/titles/thgraphs/exploring-graphs-with-elixir/](https://pragprog.com/titles/thgraphs/exploring-graphs-with-elixir/)

### SPARQL parser for Elixir (the programming language)

- [https://github.com/langens-jonathan/sparql](https://github.com/langens-jonathan/sparql)
- [https://medium.com/@tonyhammond/querying-rdf-with-elixir-2378b39d65cc](https://medium.com/@tonyhammond/querying-rdf-with-elixir-2378b39d65cc)
- Also tools written in Elixir to handle RDF data etc. Could be re-used or moved to Erlang.

### SQLAlchemy

- [https://github.com/RDFLib/rdflib-sqlalchemy](https://github.com/RDFLib/rdflib-sqlalchemy) RDFLib store using SQLAlchemy dbapi as back-end
- Python
- Zag ergens een opmerking of sparql - dat moet verder uitgezocht worden als we hier iets mee willen doen.

## Translators from SPARQL to SQL

[https://github.com/search?q=sparql+to+sql&type=repositories&p=1](https://github.com/search?q=sparql+to+sql&type=repositories&p=1)

### sparql2sql

- [https://github.com/eugenesiow/sparql2sql](https://github.com/eugenesiow/sparql2sql)
- Java - archived repo

### SPARQL-to-SQL

- New development in Java
- [https://github.com/VCityTeam/SPARQL-to-SQL](https://github.com/VCityTeam/SPARQL-to-SQL)

### Quetzal - SPARQL to SQL

- [https://github.com/LITMUS-Benchmark-Suite/sparql-to-gremlin](https://github.com/LITMUS-Benchmark-Suite/sparql-to-gremlin)
- Java research project
- Paper: [https://dl.acm.org/doi/10.1145/2463676.2463718](https://dl.acm.org/doi/10.1145/2463676.2463718)  **Building an efficient RDF store over a relational database**

### SPARQL to Gremlin

- [https://github.com/LITMUS-Benchmark-Suite/sparql-to-gremlin](https://github.com/LITMUS-Benchmark-Suite/sparql-to-gremlin)

### Papers

[https://dl.acm.org/action/doSearch?fillQuickSearch=false&target=advanced&expand=dl&field1=AllField&text1=SPARQL&field2=AllField&text2=SQL&searchArea[0]=SeriesKey&operator[0]=And&SeriesKeyAnd=&EpubDate=&EpubDate=&AfterMonth=&AfterYear=&BeforeMonth=&BeforeYear=](https://dl.acm.org/action/doSearch?fillQuickSearch=false&target=advanced&expand=dl&field1=AllField&text1=SPARQL&field2=AllField&text2=SQL&searchArea%5B0%5D=SeriesKey&operator%5B0%5D=And&SeriesKeyAnd=&EpubDate=&EpubDate=&AfterMonth=&AfterYear=&BeforeMonth=&BeforeYear=)

- **A complete translation from SPARQL into efficient SQL** [https://dl.acm.org/doi/10.1145/1620432.1620437](https://dl.acm.org/doi/10.1145/1620432.1620437)
- **Building an efficient RDF store over a relational database** [https://dl.acm.org/doi/10.1145/2463676.2463718](https://dl.acm.org/doi/10.1145/2463676.2463718)

### Notes from discussion with Marc and Rob:

Marc: Use blocks in facet templates as mapping from predicates to fields (actually broader as the blocks can be seen as functions that could also do data wrangling and access over edges)

## Over copyrights - uit het NDE document

**Update**: dit zit nu in de core module mod_copyright.

Data:

- Copyright statement
- Rechthebbende

Gebruik voor copyright statement:

- [RightsStatements.org](http://RightsStatements.org)  [https://rightsstatements.org/page/1.0/?language=nl](https://rightsstatements.org/page/1.0/?language=nl)
- Creative Commons Legal Tools  (in het geval van werken in het publieke domein of vrijgegeven als open content) [https://www.w3.org/submissions/ccREL/](https://www.w3.org/submissions/ccREL/)

Zie ook:

- [https://rightsstatements.org/files/180117requirements_for_the_technical_infrastructure_for_standardized_international_rights_statements_v1.2.1.pdf](https://rightsstatements.org/files/180117requirements_for_the_technical_infrastructure_for_standardized_international_rights_statements_v1.2.1.pdf)
- [https://opendefinition.org/](https://opendefinition.org/)

### Over verrijking en annotaties

Er is een W3C recommendation op basis van linked data: de Web Annotation-standaard
die gebruikt kan worden voor transcripties c.q. verrijking van tekstrepresentaties.

- [https://www.w3.org/TR/annotation-model/](https://www.w3.org/TR/annotation-model/)