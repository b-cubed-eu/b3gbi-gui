# Contributing to b3gbiGUI

Thank you for your interest in contributing to the B-Cubed Shiny application! This document provides guidelines for developers.

## Development Setup

### Prerequisites

- R (>= 4.1.0)
- Git
- RStudio (recommended)

### Getting Started

1. Fork the repository on GitHub
2. Clone your fork:
   ```bash
   git clone https://github.com/YOUR_USERNAME/b3gbi-gui.git
   cd b3gbi-gui
   ```
3. Install dependencies:
   ```r
   install.packages("remotes")
   remotes::install_deps(dependencies = TRUE)
   ```

## Development Workflow

See `plan/guidelines.md` for comprehensive guidelines. Summary:

### 1. Create a Branch

```bash
git checkout develop
git pull origin develop
git checkout -b feature/track-a-infrastructure
```

### 2. Make Changes

- Follow the Tidyverse style guide
- Write tests for new functionality
- Update documentation

### 3. Test Your Changes

```r
devtools::test()        # Run tests
lintr::lint_package()   # Check style
devtools::check()       # Full package check
```

### 4. Commit

```bash
git add .
git commit -m "feat(data): add caching for data loading

Implement memoization to reduce load times for repeated uploads.
Closes #123"
```

### 5. Push and Create PR

```bash
git push origin feature/track-a-infrastructure
```

Then create a Pull Request on GitHub with:
- Clear title and description
- Reference to related issues
- Screenshots (if UI changes)
- Checklist of completed items

## Code Standards

### Style Guide

- Use snake_case for names
- 2-space indentation
- Max 80 characters per line
- Document all exported functions

### Testing

- Write tests for all new functions
- Maintain >80% code coverage
- Use `testthat` for unit tests
- Use `shinytest2` for UI tests

### Documentation

- Add roxygen2 comments to functions
- Include examples in documentation
- Update README if needed

## Project Structure

This is an R package containing a Shiny app:

```
b3gbi-gui/
├── R/              # R code modules
├── tests/          # Test suite
├── man/            # Auto-generated docs
├── inst/           # Static assets
├── DESCRIPTION     # Package metadata
└── .github/        # CI/CD workflows
```

## Refactoring Project

This project is currently undergoing major refactoring. See `/plan/` directory:
- `plan.md` - Master plan with phases
- `tracks.md` - Detailed task breakdown
- `guidelines.md` - Git and coding standards

Current Phase: **Track A - Infrastructure**

## Questions?

- Open an issue for bugs or features
- Check existing issues before creating new ones
- Join discussions in existing threads

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
