<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />The code of this app is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.

# Meaning Mapper App: a lexical annotation app
This an R-Shiny app containing a lexical annotation tool created for the <a href='https://mangalamresearch.shinyapps.io/VisualDictionaryOfBuddhistSanskrit/' target='_blank'>Buddhist Translators Workbench</a>

The following guidelines are meant for the Buddhist Translators Workbench lexicographic team. This is a working prototype and very much a word in progress. 

## 1. Required file structure for local installation
On your favourite location, create a MeaningMapperRelated folder. Inside it create 3 folders: RawConc, PreEditVersions, and MeaningMapperApp. 
* 	Inside MeaningMapperApp create 2 folders: data and www . Inside MeaningMapperApp, but not nested in any other folder, put the R script containing the code of the app. The file name must be app.R
* 	Inside www store:
 * * 	LexicalData.rds, a file containing the dataset that powers our VisualDictionaryOfBuddhistSanskrit;
*  * 	the html file containing the Meaning Mapper documentation (optional).
* 	Inside data put one single file: Conc.txt. To create this file:
*  * 	go to Sketch Engine
*  * 	download concordances as .csv (options must be set to sentence view and with all agreed metadata)
*  * 	retrieve the file from downloads, move it to data and rename it Conc.txt
Finally, the BTW team need to have a shared Dropbox folder BTW_Submissions with the pre-set sub_folders.

## 2. Other requirements

Before launching from RStudio make sure you have all the required packages installed:
* 	networkD3
* 	DT
* 	tidyverse
* 	tokenizers
* 	stringr
* 	ggplot2
* 	plotly
* 	data.tree
* 	collapsibleTree
* 	DiagrammeR
* 	wordcloud
The app works on all operative systems, but UTF-8 diacritics are known to display poorly on instances of R running on Windows.

## 3. IMPORTANT: read before start annotating.

* 	what you see is what you save: whatever is displayed in the sidebar input fields is what you are going to save if you click one of the Save buttons. Example: if you have already saved 2 dep.rels (or pros.rel) and you want to add another dep.rel, you can change one of the dep.rel to a new selection + dep.head and the new information will be saved beside the existing ones (i.e. changing the selected dep.rel alone will not delete previously saved dep.rels). To delete a previously saved dep.rel/pros.rel by setting the corresponding dep.head to blank, input white space.
* 	try not to go back and forth between citations before finishing one, to minimise the risk of not saving or accidentally overwriting what you had saved. The workflow should be: start and finish 1 citation; click Save (it is a good idea to also click Save draft as you progress through the annotation fields), this writes your annotation to file (there is no undo; if you want to change what you wrote to file, use Edit). If you want to check another citation (e.g. one you have previously annotated) while annotating, consult the whole dataset table down the page (search for the citation/text you want with the Search box at the top of the table). If for any reason you do go back and forth before finishing a citation, pay attention to what is shown in the input field and in the annotation display to the right: make sure that if you are not using a dep.rel/pros.rel, the corresponding dropdown menu is set to select AND corresponding dep.head blank. At the current state of development, if the dropdown menu is set to a dep.rel (or pros.rel) and the field below is left blank, the tool fills it with the first word of the citation; if it is left like that and saved it, you will save that info, which is very likely to be rubbish.
* 	the tool assumes that you annotate the fields sequentially from top to bottom (you can of course skip fields that are not relevant to a citation) and it considers a citation complete once you input sem.pros and save. If sem.pros is blank, it is not complete. More importantly, if you fill in sem.pros and leave the rest blank, the tool will count it as complete. Once the sem.field is annotated and the citation considered complete, the citation will not appear in the citations dropdown menu in the next app session (it will remain accessible through the Edit section). So be careful: do not fill in sem.pros before you have inputted all the annotations you want to input!
* 	Edit can be used to annotate any value/column. Use with care because it overwrites the file, not just the concordance you are editing. Edit is mainly to change segmentation or values in citations other than the one currently open in the annotation editor. It should not be used to edit values that can be changed with the inputting fields above the Edit section. If you annotate something that pertains to citation currently open in the annotator (e.g. segmentation), you need to refresh the page to view the change in the annotation section of the app. You can refresh the page either by moving to another citation and back, or by using the refresh button at the very top on the page. The changes will be saved to file and be visible in the whole dataset table without need of refreshing the page.
* 	BTW team: check and edit segmentation before getting started on the annotation of a citation and refresh the page before starting with the annotation —this is very important because correcting segmentation errors will affect the word numbering and you will not see the new word numbering in the annotation section until you refresh page (you will see it in the whole dataset table).

## 4. Annotation guidelines

* 	check and edit segmentation before getting started on the annotation of a citation. When correcting segmentation error edit the minimum number of tokens necessary to correct a string (a token is a string of characters between white spaces).
* 	all annotations refer to the lemma. If a citation contains multiple instances of the lemma, all annotations must refer to the same instance. Pick the one that seems richer in semantic information. You do not have to highlight which one you pick. 
* 	long sentences need not be shortened. In the translation field only input the bit of translation relevant to understand the lemma. In the add note field, input Too Long —you can still add your own notes after that.
* 	good examples: if you think a citation provide a particularly clear example of a meaning of the lemma, in the notes field input Good Example —you can still add your own notes after that.
* 	grammatical info (lemma, case/voice and number): input them one after the other separated by comma NO SPACE. The field will be pre-populated with the stemmed form of the lemma followed by two commas. Put in the complete lemma that pertains to the citation you are annotating and complete the rest. For compounds you can either write lemma,comp,comp or leave case/voice and number blank: lemma,,. At the moment no validation is performed on the text input fields. Check for typos and make sure you follow the agreed conventions/abbreviations (as in MMWP). If you are unsure what labelling conventions you are following, you can check the values in the dictionary dataset by typing unique(LexicalData$YOUR_CHOSEN_COLUMN) in a new page in RStudio (e.g. unique(LexicalData$number); to check the names of columns: colnames(LexicalData). If you have not launched the app yet, you need to read in the LexicalData file with read.csv.
* 	stratified random sampling: at the moment the citation menu displays 100% of the concordances in ordered by title and sentence number. It is important to preserve the randomness of selection within one text: do not annotate citations sequentially.

## 5. Menus navigation

* 	click less, type less, save time: you can type any part of an item in the input menu, you don't have to start at the beginning. For example, if you have million citations from the abhidharmakośabhāṣya and you want to Edit the citation for Abhidharmakosa1879. You don't have to type 'Abhidharmakosa1879' in the input field. If you type for example 879, the app will suggest to you only citations that contain 879, which hopefully would be only a handful. Once you have typed enough to pull up your desired value from the menu use the arrow to highlight and hit enter to select it and hit tab to move to the next field.
* 	some menu choices are constrained by previous choices, for example sem.fields are suggested on the basis of the inputted domain and sem.cat are suggested on the basis of the inputted sem.field. You can always create a new value, but please consider well the options already available before creating a new one.

## 6. Track your progress

* 	annotate 20% of all citations for large datasets or a percentage that allows you to have as close as possible to min 30 citations in total for small datasets. The chart & writing at the top display the percentage on the overall set, the bar chart at the top-left displays per-text percentage. Text titles are displayed upon hovering on a bar
* 	once you complete 20% citations for a text the corresponding bar in the top-left chart fades to a pale shade. You can also check progress in the Work Summary table just before the Revision section. Texts for which you have annotated 20% are highlighted in green.
* 	 If you want to quickly check how many citations you have for a certain text, or to spot a text for which you have few citation (e.g. if you only have one hour and want to get a text done) you can look at the mid chart at top.

## 7. How to review before submitting

### Revision workflow:
1.	check senses with charts at top of page: inspect semantic tree and bar chart at the top-right of the page to spot inconsistencies and typos. For readability the semantic tree nodes are not proportional to number of citations annotated with a value. You can see the number of annotations associated with a sense/sem.field by hovering on the node. Clicking on a node will collapse it (can be useful to tidy up display of dense trees). 
2.	check the semantic info in the dataset: the Revision Section at the bottom of the app provides the semantic info you have annotated. If you want to check a citation or translation connected to an annotation use the Whole Dataset table above and search for the citation reference or value you are interested in (e.g. 'bbhumi1111' OR 'Variety@Change'). The table contains many columns, you will have to scroll horizontally to see them all. The default set of the dataset summary is to display all citations. You may display less by changing the number of rows to be shown in the menu on the top left.
3.	check synonyms: check which other lemmata have been annotated with the same semantic fields (near synonyms) and semantic categories (synonyms). Look for words you expect to find as synonyms. For example, when annotating nāmadheya, expect nāma to have the same sem.field and sem.cat and saṃjñā to share at least a sem.field with nāmadheya. If nāma does not appear in the full synonyms tree, first check whether it is in the Dictionary dataset, by consulting the table at the very end (you can search for nāma with the search field on the top-right). If it is in the dataset, check what sem.field/sem.cat it has been annotated with; look it up in the online VisualDictionary. You may want to change the annotations to match the sem.field of nāma, or to discuss changing nāma sem.field —use slack semantic_fields channel for this and include your colleagues in the post too for team discussion. Conversely, if a seemingly unrelated word pops-up in the synonym tree, it should be also checked and discussed why.
4.	Submit: once all annotations are tidy and you are happy with the set click Submit. This will create a copy of the annotated .csv and the Edit log in the BTW_Submission folder. Submissions are automatically stored in the folder, so there is no need to post it on slack. However, it is convenient to send a slack message to alert of the submission.

## 8. Cotext

Cotext wordcloud shows which cotext items a citation shares with other citations (if any). Under the wordcloud is a searchable table listing the citations that have more than one word in common with the citation that is being annotated (as well as number of shared words, percentage of that number over total word of the citation and list of shared words). 'Words' here ideally means semantically rich words, so function words and other common words (i.e. stopwords) should be removed. At the moment we are not annotating semantic info for cotext items. 
Spot incomplete annotations by looking at the chart at the top-right of the screen. With sem.pros selected (default setting) the red bar for trace0 should be all red and there should be no other red bits in other columns. If a red bit is shown somewhere, click on it to see which citation it is, and you'll know that you have semantic annotations in that citation but no sem.pros.; go back to it and complete —unless you were keeping it for later. In fact you can use this feature to flag citations to finish later, e.g. if you want to check something before submitting it.


## 9. Segmentation syntax

The convention to follow is: 
* 	STEM -suffix @ COMPOUNDEDSTEM -suffix; spaces are important.
* 	^ to separate lowercase item from next item when a vowel is lost for sandhi.
* 	avagraha is replaced with a 
* 	indeclinables are lowercase
* 	verb asti is also lowercase because impossible to reduce to stem
* 	for the time being, pronouns may be inconsistently stemmed, sometimes lower sometimes uppercase, due to difficulty in stemming
* 	there might be some inconsistency in verb derivated forms
* 	use the Segmentation notes field to report inconsistency or suggest alternative ways to segment something. Keep the segmentation notes brief.
