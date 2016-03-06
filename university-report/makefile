CORPUS := introduction.md chapter1.md chapter2.md
METADATA := metadata.yaml

report : $(CORPUS) $(METADATA)
	pandoc $(METADATA) introduction.md -f markdown -s -o university-report.pdf
