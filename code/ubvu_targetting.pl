:- module(ubvu_subset_selection, [target_ubvu_pages/2,
								  target_bible_pages/0]).

/** <module> Subset selection for annotation
*
* Code based on subset selection file in Accurator cpack, adapted to
* target page types for the ubvu.
*/
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_library)).

:- rdf_register_prefix(oa, 'http://www.w3.org/ns/oa#').
:- rdf_register_prefix(accu, 'http://accurator.nl/schema#').
:- rdf_register_prefix(edm, 'http://www.europeana.eu/schemas/edm/').

%%	target_ubvu_pages(+TargetType, +Campaign)
%
%	Targets edm works which have the specified annotation
% target_ubvu_pages('http://accurator.nl/ubvu#Target','http://accurator.nl/ubvu#Campaign').
target_ubvu_pages(TargetType, Campaign) :-
	% should be addapted to current folder structure
	rdf_load('../../ubvu_bibles/rdf/ubvu_bibles.ttl'),
	rdf_load('../rdf/page_annotations.ttl'),
	unload_graph(Campaign),
	illustrated_pages(PageAnnotations),
	maplist(target_annotation(TargetType, Campaign), PageAnnotations),
	rdf_save_turtle('../rdf/ubvu_campaign.ttl', [graph(Campaign)]).

unload_graph(Graph) :-
	(   rdf_graph(Graph)
	->  rdf_unload(Graph)
	;   true
	).

illustrated_pages([
	   'http://purl.org/vocab/nl/ubvu/FullPageIllustration',
	   'http://purl.org/vocab/nl/ubvu/IllustratedPage',
	   'http://purl.org/vocab/nl/ubvu/MultipleIllustrationsPage',
	   'http://purl.org/vocab/nl/ubvu/PartialIllustrationPage',
	   'http://purl.org/vocab/nl/ubvu/TextAndIllustrationPage'
]).

%%	target_annotation(+Annotation, +TargetType, +Campaign)
%
%	Targets edm works which have the specified annotation
% target_annotation('http://accurator.nl/bible#Target','http://accurator.nl/bible#Campaign','http://purl.org/vocab/nl/ubvu/MultipleIllustrationsPage').
target_annotation(TargetType, Campaign, Annotation) :-
	Options = [target_type(TargetType), campaign(Campaign),
			  targetter('http://accurator.nl/user#AnnotationScanner')],
	%find all works with annotatoin
	findall(Object,
			(	rdf(AnnotationUri, oa:hasBody, Annotation),
				rdf(AnnotationUri, oa:hasTarget, Object),
				rdf(Object, rdf:type, edm:'ProvidedCHO')),
			Objects),
	length(Objects, NumberObjects),
	debug(tag_works, 'Number of works with ~p annotation: ~p',
		  [Annotation, NumberObjects]),
	maplist(campaign_nomination(Options), Objects).

target_bible_pages :-
	Graph = 'http://purl.org/collections/nl/ubvu/ubvu_bibles.ttl',
	TargetLeon = 'http://accurator.nl/page#TargetLeon',
	TargetCristina = 'http://accurator.nl/page#TargetCristina',
	TargetSebastien = 'http://accurator.nl/page#TargetSebastien',
	TargetChris = 'http://accurator.nl/page#TargetChris',
	findall(Page,
			rdf(Page, rdf:type, edm:'ProvidedCHO', Graph),
			Pages),
	length(Pages, NumberPages),
	forall(between(0,250,X1),
		   (     nth0(X1, Pages, Target),
				 rdf_assert(Target, rdf:type, TargetLeon, 'pageTargets'),
				 debug(pages, 'Targetting ~p', [Target]))),
	forall(between(251,500,X2),
		   (     nth0(X2, Pages, Target),
				 rdf_assert(Target, rdf:type, TargetCristina, 'pageTargets'),
				 debug(pages, 'Targetting ~p', [Target]))),
	forall(between(501,750,X3),
		   (     nth0(X3, Pages, Target),
				 rdf_assert(Target, rdf:type, TargetSebastien, 'pageTargets'),
				 debug(pages, 'Targetting ~p', [Target]))),
	forall(between(750,1002,X4),
		   (     nth0(X4, Pages, Target),
				 rdf_assert(Target, rdf:type, TargetChris, 'pageTargets'),
				 debug(pages, 'Targetting ~p', [Target]))),
	debug(pages, 'number pages: ~p', [NumberPages]).

%%	campaign_nomination(+Options, +Work)
%
%	Nominate a work to be in a campaign and record who targetted this
%	work.
campaign_nomination(Options, Work) :-
	option(target_type(TargetType), Options),
	option(targetter(Targetter), Options),
	%Don't assert when already nomminated
	rdf(Work, rdf:type, TargetType),
	rdf(Work, accu:targetedBy, Targetter), !.
campaign_nomination(Options, Work) :-
	option(target_type(TargetType), Options),
	option(targetter(Targetter), Options),
	option(campaign(Campaign), Options),
	rdf_assert(Work, rdf:type, TargetType, Campaign),
	rdf_assert(Work, accu:targetedBy, Targetter, Campaign).

