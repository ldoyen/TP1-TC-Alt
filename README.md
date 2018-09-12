Pour faire fonctionner la récupération de l'avancement des étudiants avec google forms, il faut:


1. Create a new form by clicking "New" on https://drive.google.com
2. Name your form something memorable.
3. Your form should have only one question which should be a paragraph type 
question.
4. In the upper right corner of the form there should be three vertical white
dots. Click on that icon and then click "Get pre-filled link." This will open
a new window.
5. Dans le champs réponse tapez toto, puis cliquer sur "obtenir le lien" et copier le lien.
6. Paste the generated link into `customTests.R` where indicated, so that a
string containing the link is assigned to the `pre_fill_link` variable in the
function `submit_log()`. Enlever toto à la fin du lien

Pour décoder les réponses des étudiants:

1. Download the `csv` results from Google Forms.
2. Sourcer mon script `mon_google_form_decode.R`
3. Run `mon_google_form_decode()` and select the `csv` you downloaded.
4. `google_form_decode()` will return a data frame containing how each of your
students performed on every question.
