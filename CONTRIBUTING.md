# Contributing Guidelines

*Pull requests, bug reports, and all other forms of contribution are welcomed and highly encouraged!*

### Contents

- [Code of Conduct](#book-code-of-conduct)
- [Asking Questions](#paperclip-asking-questions)
- [Acceptable Types](#whale-acceptable-types)
- [Opening an Issue](#fishing_pole_and_fish-opening-an-issue)
- [Bug Reports and Other Issues](#blowfish-bug-reports-and-other-issues)
- [Feature Requests](#tropical_fish-feature-requests)
- [Creating a Branch](#anchor-creating-a-branch)
- [Writing Commit Messages](#memo-writing-commit-messages)
- [Submitting Pull Requests](#fish_cake-submitting-pull-requests)
- [Coding Style](#shark-coding-style)
- [Certificate of Origin](#crab-certificate-of-origin)
- [Credits](#pray-credits)

> **This guide serves to set clear expectations for everyone involved with the project so that we can improve it together while also creating a welcoming space for everyone to participate. Following these guidelines will help ensure a positive experience for contributors and maintainers.**

## :book: Code of Conduct

Please review our [Code of Conduct](CODE_OF_CONDUCT.md). It is in effect at all times. We expect it to be honored by everyone who contributes to this project.

## :paperclip: Asking Questions

GitHub issues are not the appropriate place to debug your specific project, but should be reserved for filing bugs and feature requests.

## :whale: Acceptable Types

Throughout this project, we use standard `types` to communicate more effectively. These types are used when labeling issues, creating branches, committing changes and naming pull requests. Please reference and use the `types` below when contributing to this project:

- **fix:** Patch an unexpected or undesirable behavior

- **build:** Add or modify build-related components (e.g. workflows, versioning, etc.)

- **chore:** Maintenance of product or repository (e.g. gitignore, templates, etc.)

- **docs:** Add or modify documentation

- **feature:** Add or modify a feature or functionality

- **refactor:** Rewrite or restructure code without altering behavior

- **style:** Reformat code style without altering behavior

- **test:** Add or modify tests

All issues, branches, commits and pull requests should align with one of the above `types`. If you have questions or need assistance, please reach out to a maintainer.

## :fishing_pole_and_fish: Opening an Issue

Before [creating an issue](https://help.github.com/en/github/managing-your-work-on-github/creating-an-issue), check that you are using the latest version of the project. If you are not up-to-date, see if updating fixes your issue first.

### :blowfish: Bug Reports and Other Issues

A great way to contribute to the project is to send a detailed issue when you encounter a problem.
This repo has a 'Bug Report' issue template that will guide you in submitting a well-written, thorough bug report. If possible, please also create a [reprex](https://reprex.tidyverse.org/) and include it in your issue. This helps us quickly identify and fix the problem.

When opening an issue, please follow these guidelines:

- **Review the documentation** before opening a new issue.

- **Be specific.** Describe the problem in detail. What did you expect to happen? What actually happened? What were you doing when the problem occurred? What version of the library are you using? What version of the OS are you running?

- **Provide a reproducible example (e.g. [reprex](https://reprex.tidyverse.org/))** If possible, provide a minimal, complete, and verifiable example that reproduces the issue. This is often the most important part of a bug report. If you can provide a sample project that reproduces the issue, that is even better!

- **Prefer using [reactions](https://github.blog/2016-03-10-add-reactions-to-pull-requests-issues-and-comments/)**, not comments, if you simply want to "+1" an existing issue.

- **Use [GitHub-flavored Markdown](https://help.github.com/en/github/writing-on-github/basic-writing-and-formatting-syntax).** Especially put code blocks and console outputs in backticks (```). This improves readability. In short, since you are most likely a developer, **provide a ticket that you would like to receive**.

- **Do not open a duplicate issue!** Search through existing issues to see if your issue has previously been reported. If your issue exists, comment with any additional information you have. You may simply leave a reaction to bump the issue, which helps prioritize the most common problems and requests. 

- **Fully complete the provided issue template.** The bug report template requests all the information we need to quickly and efficiently address your issue. Be clear, concise, and descriptive. Provide as much information as you can, including steps to reproduce, stack traces, compiler errors, library versions, OS versions, and screenshots (if applicable).

### :tropical_fish: Feature Requests

Feature requests are more than welcome! While we will consider all requests, we cannot guarantee your request will be accepted or provide the timeline for implementation and release. 

- **Do not open a duplicate feature request.** Search for existing feature requests first. If you find your feature (or one very similar) previously requested, comment on or add a reaction to that issue.

- **Fully complete the provided issue template.** The 'Feature Request' template asks for all necessary information for us to begin a productive conversation. 

- **Be precise** about the proposed outcome of the feature and how it relates to existing features. Include all implementation details.

## :anchor: Creating a Branch

Many contributions to this project will make use of [branching](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-branches). This repo has two permanent branches: `main` and `dev`. To contribute via branching, you should create a branch from and pull request to the `dev` branch. Your branch name should follow these conventions:

- Use `type/i#-describe-branch-purpose`, where # is the number of the target issue
- Prefix with the `type` of change the branch will introduce (see [acceptable `types`](#whale-acceptable-types) above)
- Include an issue number in your branch name (your changes should relate to an [existing issue](#fishing_pole_and_fish-opening-an-issue))
- Provide a clear but concise description of the branch's purpose

## :memo: Writing Commit Messages

Please write a **conventional** commit message:

1. Use `type(optional scope): commit message` structure (see above for [acceptable `types`](#whale-acceptable-types))
1. Separate changes into their own commits with unique messages
1. Write using imperative mood (example: "fix: correct the food web")
1. Include a `scope` if a commit serves a larger purpose (example: "refactor(food web fix): reorder data ingestion")
1. Be as brief as possible without losing clarity

## :fish_cake: Submitting Pull Requests

We appreciate pull requests! Before [forking the repo](https://help.github.com/en/github/getting-started-with-github/fork-a-repo) and [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/proposing-changes-to-your-work-with-pull-requests) for non-trivial changes, it is usually best to first open an issue to discuss the changes, or discuss your intended approach for solving the problem in the comments for an existing issue.

*Note: All contributions will be licensed under the project's license.*

**Guidelines for happy pull requests:**

- **Title meaningfully.** Consistent with the convention elsewhere, please title your pull request: "Type(#i): Describe Change Purpose", where "i" is the number of the issue addressed by the PR. Please reference the [acceptable types](#whale-acceptable-types) above.

- **Request appropriate reviewers.** At the very least, please request review from one or all of the project maintainers (maintainers are listed on the [README](README.md)). You should include additional reviews from people familiar with the subject or problem your proposed changes address.

- **Communication is the key to success.** If you are unsure about something, ask! We are happy to help. We have an open channel of communication, make sure to reach out and further develop your ideas or changes before working on a pull request. 

- **Smaller is better.** Submit **one** pull request per bug fix or feature. A pull request should contain isolated changes pertaining to a single bug fix or feature implementation. **Do not** refactor or reformat code that is unrelated to your change. It is better to **submit many small pull requests** rather than a single large one. Enormous pull requests will take enormous amounts of time to review, or may be rejected altogether. 

- **Coordinate bigger changes.** For large and non-trivial changes, open an issue to discuss a strategy with the maintainers. Otherwise, you risk doing a lot of work for nothing!

- **Prioritize understanding over cleverness.** Write code **clearly** and **concisely**, please supply comments when it is needed. Remember that source code usually gets written once and read often. Ensure the code is clear to the reader. The purpose and logic should be obvious to a reasonably skilled developer, otherwise you should add a comment that explains it.

- **Follow the existing architecture.** If you are adding new functionality, try to follow the existing architecture and patterns in the code base. If you are unsure, ask for guidance.

- **Include test coverage.** Add unit tests or UI tests when possible. Follow existing patterns for implementing tests.

- **Update the example project** if one exists to exercise any new functionality you have added.

- **Add documentation.** Document your changes with code comments or other documentation.

- **Use the correct branch.** Branch from and [submit your pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork) to the correct branch (see above for [branching instructions](#anchor-creating-a-branch)). In this repo, it will be the `dev` branch.

- **[Resolve any merge conflicts](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/resolving-a-merge-conflict-on-github)** that occur.

## :shark: Coding Style

Consistent formatting is an essential part of effective collaboration and communication. Reading, writing and reviewing code all benefit from following the standards and conventions of a project. Failure to do so will result in a prolonged review process that has to focus on updating the superficial aspects of your code, rather than improving its functionality and performance. This repo uses and adheres to the [tidyverse style guide](https://style.tidyverse.org/) and the [Air autoformatting tool](https://posit-dev.github.io/air/) that follows it. Please read and follow these guides as you make contributions to the project.

## :crab: Certificate of Origin

*Developer's Certificate of Origin 1.1*

By making a contribution to this project, I certify that:

> 1. The contribution was created in whole or in part by me and I have the right to submit it under the open source license indicated in the file; or
> 1. The contribution is based upon previous work that, to the best of my knowledge, is covered under an appropriate open source license and I have the right under that license to submit that work with modifications, whether created in whole or in part by me, under the same open source license (unless I am permitted to submit under a different license), as indicated in the file; or
> 1. The contribution was provided directly to me by some other person who certified (1), (2) or (3) and I have not modified it.
> 1. I understand and agree that this project and the contribution are public and that a record of the contribution (including all personal information I submit with it, including my sign-off) is maintained indefinitely and may be redistributed consistent with this project or the open source license(s) involved.

## :fish: Thank You!

If you are reading this, thank you! We appreciate your interest in contributing to this project.

To confirm that you have read this guide and are following it as best as possible, **include this emoji at the top** of your issue or pull request: :fish: `:fish:`

## :pray: Credits

This document was inspired by [@jessesquires](https://github.com/jessesquires). 
