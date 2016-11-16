:- module(export_ubvu_csv, [ export_csv/0 ]).

% Get core libraries

:- use_module(library(semweb/rdf_db)).		% Core RDF
:- use_module(library(semweb/rdf_library)).	% Load RDF from library
:- use_module(library(csv)).

:- rdf_register_ns(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_ns(oa, 'http://www.w3.org/ns/oa#').
:- rdf_register_ns(ann_ui, 'http://semanticweb.cs.vu.nl/annotate/ui/').
:- rdf_register_ns(edm, 'http://www.europeana.eu/schemas/edm/').
:- rdf_register_ns(ubvu, 'http://purl.org/collections/nl/ubvu/').

:- rdf_meta
	label(-, r, -, -),
    concepts(r, -).

%%	export_csv
%
%	export the page annotations to a csv file.
export_csv :-
	FilePath = '../csv/page_annotations.csv',
	load_data('../../ubvu_bibles/', 'UBVU-bibles'),
	load_data('../../ubvu_bibles/', 'UBVU-bible-page-types'),
    load_data('../rdf/', 'UBVU-page-annotations'),
	select_annotated_objects(Objects),
	write_annotations(FilePath, Objects).

%%	load_data(+Path, +List)
%
%	Load the data from a library.
load_data(Path, List) :-
	rdf_attach_library(Path),
    rdf_load_library(List).

%%	select_annotated_objects(-Annotations)
%
%	Get all annotated objects.
select_annotated_objects(Objects) :-
	setof(Object, Annotation^
		  (	  rdf(Annotation, rdf:type, oa:'Annotation'),
			  rdf(Annotation, oa:hasTarget, Object)
		  ),
		  Objects).

%%	write_annotations(+FilePath, +Objects)
%
%	We do not export all annotations, just the objects which are
%	annotated (refraining from having duplicate info).
write_annotations(FilePath, Objects)  :-
	Header = row('object_id', 'annotation_type', 'annotation_text', 'annotation_uri', 'fragment'),
	setup_call_cleanup(
		open(FilePath, write, Out),
		(
			csv_write_stream(Out, [Header], []),
			maplist(write_annotation(Out), Objects)
		),
		close(Out)
	).

%%	write_annotation(+Out, +Object)
%
%	Write annotated object to csv stream.
write_annotation(Out, Object) :-
	get_data_annotation(Object, ObjectId, Type, Text, Uri),
	csv_write_stream(Out, [row(ObjectId, Type, Text, Uri, null)], []).

%%	get_data_annotation(+Object, -ObjectId, -Type, -Text, -Concept)
%
%	Retrieve information about an annotated object.
get_data_annotation(Object, ObjectId, Type, Text, Concept) :-
	rdf(Annotation, oa:hasTarget, Object),
	rdf(Object, rdf:type, edm:'ProvidedCHO'),
	rdf(Annotation, oa:hasBody, Concept), !,
	rdf(Concept, skos:prefLabel, literal(lang(nl, Text))),
	string_concat('http://purl.org/collections/nl/ubvu/print-', ObjectId, Object),
	Type = 'Page type'.

