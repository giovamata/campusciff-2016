"""
@author: U{Nines Sanguino}
@version: 0.2
@since: 20Jun2015
"""

__version__ = '0.2'
__modified__ = '20Jun2015'
__author__ = 'Nines Sanguino'
from SPARQLWrapper import SPARQLWrapper, JSON, XML, RDF
import xml.dom.minidom



def getLocalLabel (instancia):

 	sparqlSesame = SPARQLWrapper("http://localhost:8080/openrdf-sesame/repositories/SocialNetwork",  returnFormat=JSON)
	queryString = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX sn:  <http://ciff.curso2015/ontologies/owl/socialNetwork#> SELECT ?label WHERE { sn:" + instancia + " rdfs:label ?label }"

	sparqlSesame.setQuery(queryString)
	sparqlSesame.setReturnFormat(JSON)
	query   = sparqlSesame.query()
	results = query.convert()
	devolver = []
	for result in results["results"]["bindings"]:
		label = result["label"]["value"]
		if 'xml:lang' in result["label"]:
			lang = result["label"]["xml:lang"]
		else:
			lang = None
		print "The label: " + label
		if 'xml:lang' in result["label"]:
			print "The lang: " + lang
		devolver.append((label, lang))
	return devolver

def InsertLocal(InsCmd):
	sparqlSesame = SPARQLWrapper("http://localhost:8080/openrdf-workbench/repositories/SocialNetwork/update", returnFormat=XML)
	sparqlSesame.setQuery(InsCmd)
	sparqlSesame.method = 'POST'
	sparqlSesame.setReturnFormat(XML)
	query   = sparqlSesame.query()
	results = query.convert()

	

	
def getDBpediaResource (label, lang, endpoint):
	PrefixString="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
	PrefixString=PrefixString+"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
	PrefixString=PrefixString+"PREFIX foaf: <http://xmlns.com/foaf/0.1/>"

	sparqlDBPedia = SPARQLWrapper(endpoint)
	if (lang):
		queryString = PrefixString + "SELECT ?s ?bdate ?bname WHERE { ?s rdfs:label \"" + label + "\"@" +lang + ". ?s rdf:type foaf:Person. " + "?s dbo:birthDate ?bdate. ?s dbo:birthName ?bname} " 
	else:
		queryString = PrefixString + "SELECT ?s ?bdate ?bname WHERE { ?s rdfs:label \"" + label + "\" . ?s rdf:type foaf:Person. " + "?s dbo:birthDate ?bdate. ?s dbo:birthName ?bname} " 
		
	print queryString
	print "------"
	sparqlDBPedia.setQuery(queryString)
	sparqlDBPedia.setReturnFormat(JSON)
	query   = sparqlDBPedia.query()
	results = query.convert()
#	import pdb
#	pdb.set_trace()
	for result in results["results"]["bindings"]:
		resource = result["s"]["value"]
		resbdate = result["bdate"]["value"]
		resbname = result["bname"]["value"]
		print "->The resource: " + resource + " Fecha de Nacimiento: " + resbdate + " Nombre de Nacimiento: " + resbname

# Procediendo a Insertar
		Insertcmd="PREFIX sn:  <http://ciff.curso2015/ontologies/owl/socialNetwork#> "
		Insertcmd=Insertcmd + "PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#> "
		Insertcmd=Insertcmd + "PREFIX foaf: <http://xmlns.com/foaf/0.1/> "
		Insertcmd=Insertcmd + "PREFIX dbo: <http://dbpedia.org/ontology/abstract> "
		
		Insertcmd=Insertcmd + "INSERT DATA { GRAPH <http://ciff.curso2015/ontologies/owl/socialNetwork#> {sn:instancia1 " 
		Insertcmd=Insertcmd + "dbo:birthDate \"" + resbdate + "\". sn:instancia1 dbo:birthName \"" + resbname + "\" } }"
		resinsert = InsertLocal(Insertcmd);	
		
		
def getLinkedmdbResource (label, lang, endpoint):

	PrefixString="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
	PrefixString=PrefixString+"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
	PrefixString=PrefixString+"PREFIX foaf: <http://xmlns.com/foaf/0.1/>"
	PrefixString=PrefixString+"PREFIX movie: <http://data.linkedmdb.org/resource/movie/>"
	PrefixString=PrefixString+"PREFIX lang: <http://www.lingvoj.org/lingvo/>"
	PrefixString=PrefixString+"PREFIX dc: <http://purl.org/dc/terms/>"

	sparqlLinkedmdb = SPARQLWrapper(endpoint)

	if (lang):
		queryString = PrefixString +  "SELECT ?s ?mtype WHERE { ?s rdfs:label \"" + label + "\"" + ". ?s movie:language lang:" + lang + ". ?s rdf:type ?mtype} " 
	else:
		queryString = PrefixString +  "SELECT ?s ?mtype WHERE { ?s rdfs:label \"" + label + "\" . ?s rdf:type ?mtype} " 
	
#	print queryString
#	print "------"
#	import pdb
#	pdb.set_trace()
	
	sparqlLinkedmdb.setQuery(queryString)
	sparqlLinkedmdb.setReturnFormat(JSON)
	query   = sparqlLinkedmdb.query()
	results = query.convert()

	for result in results["results"]["bindings"]:
		resource = result["s"]["value"]
		resmtype = result["mtype"]["value"]
		print "->The resource: " + resource + " Clasificion " + resmtype
		
		Insertcmd="PREFIX sn:  <http://ciff.curso2015/ontologies/owl/socialNetwork#> "
		Insertcmd=Insertcmd + "PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#> "
		Insertcmd=Insertcmd + "PREFIX foaf: <http://xmlns.com/foaf/0.1/> "
		
		Insertcmd=Insertcmd + "INSERT DATA { GRAPH <http://ciff.curso2015/ontologies/owl/socialNetwork#> {sn:instancia3 " 
		Insertcmd=Insertcmd + "rdf:type \"" + resmtype + "\" } }"
		resinsert = InsertLocal(Insertcmd);			

def getWebenemasunoResource (label, lang, endpoint):

	PrefixString="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
	PrefixString=PrefixString+"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
	PrefixString=PrefixString+"PREFIX foaf: <http://xmlns.com/foaf/0.1/>"
	PrefixString=PrefixString+"PREFIX opmopviajero: <http://webenemasuno.linkeddata.es/ontology/OPMO/>"
	PrefixString=PrefixString+"PREFIX dcterms: <http://purl.org/dc/terms/rightsHolder>"
	PrefixString=PrefixString+"PREFIX sioc: <http://rdfs.org/sioc/ns#>" 
	
	sparqlWebenemasuno = SPARQLWrapper(endpoint)
	if (lang):
		queryString = PrefixString +  "SELECT ?s ?created_at WHERE { ?s sioc:title \"" + label + "\". ?s opmopviajero:language \"" +lang + "\".?s sioc:created_at ?created_at} "

	else:
		queryString = PrefixString +  "SELECT ?s ?created_at WHERE { ?s sioc:title \"" + label + "\".?s sioc:created_at ?created_at} "


#	print "------"
#	print queryString

#	import pdb
#	pdb.set_trace()
		
	sparqlWebenemasuno.setQuery(queryString)
	sparqlWebenemasuno.setReturnFormat(JSON)
	query   = sparqlWebenemasuno.query()
	results = query.convert()

	for result in results["results"]["bindings"]:
		resource = result["s"]["value"]
		rescreated_at = result["created_at"]["value"]
		print "->The resource: " + resource + " Fecha de Creacion: " + rescreated_at 
		
		Insertcmd="PREFIX sn:  <http://ciff.curso2015/ontologies/owl/socialNetwork#> "
		Insertcmd=Insertcmd + "PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#> "
		Insertcmd=Insertcmd + "PREFIX foaf: <http://xmlns.com/foaf/0.1/> "
		Insertcmd=Insertcmd + "PREFIX sioc: <http://rdfs.org/sioc/ns#>"
		
		Insertcmd=Insertcmd + "INSERT DATA { GRAPH <http://ciff.curso2015/ontologies/owl/socialNetwork#> {sn:instancia4 " 
		Insertcmd=Insertcmd + "sioc:created_at \"" + rescreated_at + "\" } }"
		resinsert = InsertLocal(Insertcmd);			
		

if __name__ == '__main__':

	# getLocalLabel devuelve una lista con todas las instancias que haya con sus label y lang
	# y luego se hace la llamada al repositorio externo para enriquecer cada combinacion de label-lang recibida
	lista = getLocalLabel("instancia1");
	print "Lista --->  " + str(lista) + "\n"
	endpoint = 'http://dbpedia.org/sparql';
	for result in lista:
		(label, lang) = result
		if(lang):
			print "Consulta Label ---->  " + label + " Lenguaje ---->  " + lang		
		else:
			print "Consulta Label (sin lenguaje) ---->  " + label

		resource = getDBpediaResource (label, lang, endpoint);

	print "\n---------------------\n"

	lista = getLocalLabel("instancia3");
	print "Lista --->  " + str(lista) + "\n"
	endpoint = 'http://data.linkedmdb.org/sparql';

	for result in lista:
		(label, lang) = result
		if(lang):
			print "Consulta Label ---->  " + label + " Lenguaje ---->  " + lang		
		else:
			print "Consulta Label (sin lenguaje) ---->  " + label

		resource = getLinkedmdbResource (label, lang, endpoint);

	print "\n---------------------\n"

	lista = getLocalLabel("instancia4");
	print "Lista --->  " + str(lista) + "\n"
	endpoint = 'http://webenemasuno.linkeddata.es/sparql';
	for result in lista:
		(label, lang) = result
		if(lang):
			print "Consulta Label ---->  " + label + " Lenguaje ---->  " + lang		
		else:
			print "Consulta Label (sin lenguaje) ---->  " + label
		resource = getWebenemasunoResource (label, lang, endpoint);
	
	print "\n--- Fin ---\n"

		
