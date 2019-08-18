# Jupyter integration
Copy kernel.* to ~/.local/share/jupyter/kernels/tensorkernel
Modify kernel.json to point to the relevant paths

Optionally copy jupyter_notebook_config.py to ~/.jupyter/ to disable MathJax to add interactive hover hints to expressions. MathJax destroys the class info in the MathML elements.
