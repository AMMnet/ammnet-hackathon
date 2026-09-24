# Session 11: DHIS2 From Scratch

Materials for the AMMnet session on September 30, 2026, prepared and
facilitated by Mohamed Sillah Kanu.

## Start the lesson

Open [`lesson/index.html`](lesson/index.html) in a web browser and follow the
18 topics in order. The lesson is self-contained: its screenshots, styles, and
navigation are embedded in the HTML files.

## Local DHIS2 setup

The two files under [`setup/`](setup/) run DHIS2 2.42.1 and PostGIS with
Docker Compose, as described in Topic 2. They are copied from the author's
[`dhis2-local-set-up`](https://github.com/mohamedsillahkanu/dhis2-local-set-up)
repository. Its license is included in the same folder.

Participants need Docker with at least 4 GB of memory assigned, about 5 GB of
free disk space, Git or a ZIP download, and a modern web browser. Change the
default DHIS2 administrator password after the first login. This configuration
is for local training, not production use.

## Known missing files

Topic 3 refers to `csv/orgunits_import.csv` and
`csv/orgunit_groups_template.csv`. These files were not present in the source
repository supplied for publication. The organisation-unit exercise can still
be completed manually, but its CSV-import route requires those files.

## Source and permission

The rendered lesson was copied from the author's
[`AMMnet-test-Hackathon`](https://github.com/mohamedsillahkanu/AMMnet-test-Hackathon)
repository. AMMnet has the author's permission to republish these materials.
