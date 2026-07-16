# How to create and publish a blog post

This guide explains how to add a post to the
[AMMnet Hackathon blog](https://ammnet.github.io/ammnet-hackathon/) using
Quarto and GitHub.

## Use an AI assistant

This repository includes an
[`add-ammnet-blog-post`](.agents/skills/add-ammnet-blog-post/SKILL.md) skill
for turning already-prepared session materials into a blog contribution. In
Codex, open this repository and ask:

```text
Use $add-ammnet-blog-post to publish the prepared materials at SOURCE.
```

Replace `SOURCE` with a local folder or repository URL. The assistant will
inspect the materials, ask for missing publication facts, create only the key
participant files, validate the site, and prepare a GitHub handoff. Review the
post and selected materials before approving a commit or pull request. The
skill is not intended to develop unfinished lessons or invent scientific
content.

## How publishing works

- The editable website source is on the `main` branch.
- Blog posts live in `posts/`.
- The blog home page finds new posts automatically; do not add a post to
  `index.qmd` by hand.
- Contributors work on a branch and open a pull request to `main`.
- When a pull request is merged, GitHub Actions renders the website and
  publishes it to the `gh-pages` branch.
- Do not edit or commit directly to `gh-pages`. It contains generated files.

## Before you start

You need:

1. A [GitHub account](https://github.com/signup).
2. [Git](https://git-scm.com/downloads) installed on your computer.
3. [Quarto](https://quarto.org/docs/get-started/) installed if you want to
   preview the post locally or if the post contains executable code.
4. R, Python, or another required language and its packages if the post
   executes code in that language.

You can check the command-line tools with:

```bash
git --version
quarto --version
```

## 1. Get a local copy of the repository

### If you can create branches in the AMMnet repository

```bash
git clone https://github.com/AMMnet/ammnet-hackathon.git
cd ammnet-hackathon
git switch main
git pull --ff-only
```

### If you cannot create branches in the AMMnet repository

On GitHub, open the
[AMMnet repository](https://github.com/AMMnet/ammnet-hackathon), select
**Fork**, and create a fork in your account. Then run:

```bash
git clone https://github.com/YOUR-USERNAME/ammnet-hackathon.git
cd ammnet-hackathon
git remote add upstream https://github.com/AMMnet/ammnet-hackathon.git
git fetch upstream
git switch main
git merge --ff-only upstream/main
```

Replace `YOUR-USERNAME` with your GitHub username.

## 2. Create a branch

Use a short, descriptive branch name:

```bash
git switch -c add-topic-name-post
```

Make one branch and one pull request for each post.

## 3. Create the post folder

Create a lowercase folder under `posts/`. Use hyphens between words and do
not use spaces. Put the post in a file named `index.qmd`:

```text
posts/
└── topic-name/
    ├── index.qmd
    ├── featured-image.png
    └── other-files-used-by-the-post
```

Using `index.qmd` gives the post a clean URL:

```text
https://ammnet.github.io/ammnet-hackathon/posts/topic-name/
```

Images, PDFs, and small data files used only by the post should normally go
in the same folder. Keep large files out of Git when possible and link to a
stable external location instead.

If a live session has code or data that participants should download and use
outside the blog, keep those workshop materials in a separate, numbered
top-level session folder, following the existing `01_data-vis/`,
`02_data-wrangle/`, and similar folders. Ask a maintainer for the next session
number, then link to the materials from the post.

## 4. Start with this template

Create `posts/topic-name/index.qmd` and copy this example. Remove optional
fields that you do not need.

```yaml
---
title: "Live Session 9: Clear, descriptive title"
subtitle: "Optional short subtitle"
description: "One or two sentences shown in previews and search results."
author:
  - First Author
  - Second Author
date: "2026-07-16"
image: "featured-image.png"
image-alt: "A concise description of the featured image"
categories:
  - R
  - Data visualization
  - Live session
draft: true
format:
  html:
    toc: true
editor:
  markdown:
    wrap: 72
---

Write a short introduction that tells readers what the session covers and
what they will be able to do afterward.

## Overview

Add the post content here.
```

Use a fixed date in `YYYY-MM-DD` format. Do not use a dynamic value such as
`today`, because the blog is sorted by date. Reuse the spelling and
capitalization of existing categories when they describe the same topic.

Keep `draft: true` while the post is incomplete. Drafts appear during local
preview but are excluded from the published site. Remove the line when the
post is ready to publish.

## 5. Add content and media

Write normal content in Markdown. Start main sections with `##`; the post
title from the YAML header is already the page's top-level heading.

### Images

Always provide useful alternative text:

```markdown
![Map showing malaria prevalence by region](malaria-map.png)
```

For a figure produced by a code cell, use a Quarto figure description:

````markdown
```{r}
#| label: fig-malaria-map
#| fig-alt: "Map showing malaria prevalence by region"

# Code that creates the map
```
````

### Video

Embed a YouTube video with its embed URL:

```markdown
{{< video https://www.youtube.com/embed/VIDEO-ID >}}
```

### Links and downloads

Use descriptive link text rather than “click here”:

```markdown
[Download the workshop slides](workshop-slides.pdf)
```

### Code

Give executable code cells meaningful labels and hide warnings or messages
when they do not help the reader:

````markdown
```{r}
#| label: load-data
#| warning: false
#| message: false

library(tidyverse)
```
````

To show example code without running it, add `#| eval: false`. If a post
should not execute any of its code, add this to the YAML header:

```yaml
execute:
  enabled: false
```

Quarto normally runs code relative to the folder containing `index.qmd`, so
use paths such as `data/example.csv` for files inside the post folder.

## 6. Preview and check the post

To browse the site while editing, run this from the repository root:

```bash
quarto preview
```

Draft posts are visible in this local preview. When the post is ready, remove
`draft: true`, stop the preview, and explicitly render the new or changed
post:

```bash
quarto render posts/topic-name/index.qmd
```

This step is especially important for posts with executable code. The
repository freezes post output so GitHub Actions can rebuild the complete
site without installing every historical R or Python dependency. Explicitly
rendering the post runs its code and updates its files under `_freeze/`.
Commit those updated frozen files with the post.

Before submitting the post, check that:

- The title, authors, date, and categories are correct.
- `draft: true` has been removed.
- The introduction clearly states the post's purpose.
- Commands work in a clean session and code cells finish without errors.
- Images load and have meaningful alternative text.
- Links and downloads work.
- No private, sensitive, copyrighted, or very large files were added.
- The post appears on the home page and opens at the expected URL.
- Only source files and required `_freeze/` output are included; `_site/`
  should not be committed.

## 7. Commit and push the branch

Review the changes before committing:

```bash
git status
git diff --check
git add posts/topic-name _freeze/posts/topic-name
git diff --cached
git commit -m "Add topic name blog post"
git push -u origin add-topic-name-post
```

If the post has no `_freeze/posts/topic-name` folder, omit that path from the
`git add` command. Also add any separate workshop-material folder if the post
introduces one.

## 8. Open a pull request

1. Open your repository on GitHub after pushing the branch.
2. Select **Compare & pull request**.
3. Set the destination to `AMMnet/ammnet-hackathon` and the base branch to
   `main`.
4. Give the pull request a clear title and briefly describe the session,
   local checks performed, and any special software needed to rerun the code.
5. Select **Create pull request**.
6. Respond to review comments by editing the same branch, committing, and
   pushing again. The pull request updates automatically.

Do not publish the site manually. After a maintainer approves and merges the
pull request, the **Quarto Publish** workflow renders the site and updates
`gh-pages`. Follow the workflow in the repository's **Actions** tab. When it
finishes, verify the post on the
[published blog](https://ammnet.github.io/ammnet-hackathon/).

## Troubleshooting

### The post does not appear on the home page

Confirm that the file is under `posts/`, its YAML header has a valid fixed
`date`, and `draft: true` has been removed.

### A local render tries to run old posts

Render only the post you changed:

```bash
quarto render posts/topic-name/index.qmd
```

The deployment workflow uses the committed frozen results when it renders the
complete site.

### The GitHub Actions deployment fails

Open **Actions**, select the failed **Quarto Publish** run, and read the first
failed step. Correct the source or frozen output on a new branch and open
another pull request. Do not edit `gh-pages` to repair a failed build.

### Your branch is behind `main`

For a branch in the AMMnet repository:

```bash
git fetch origin
git rebase origin/main
git push --force-with-lease
```

For a branch in a fork, replace `origin/main` with `upstream/main`.

If other people share the branch, coordinate before rebasing because a rebase
rewrites its commit history.

## Maintainer note: automatic publishing

The workflow at `.github/workflows/publish.yml` runs on every push to `main`,
including a merged pull request, and can also be started manually. It uses the
official Quarto GitHub Actions to render the project and publish the generated
site to `gh-pages`.

Repository settings must allow GitHub Actions **Read and write permissions**,
and GitHub Pages must publish from the root of the `gh-pages` branch.
