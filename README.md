# shiny_9snr
R Shiny code to interactively display results of a search for gravitational-waves in supernova remnants

---

This Shiny app provides a means of interacting with the results of a search in LIGO data for continuous gravitational waves from neutron star candidates in supernova remnants. It's currently hosted on R Studio's Shiny gallery here: https://rainta.shinyapps.io/9snr-app/ 

This is a cool way to look at data that is usually presented as a dry, dusty single figure with upper limits set on the gravitational wave strain graphed against frequency. What you don't get with these is a sense of the structure inherent in some frequency regions. For example, there are a number of frequency regions in the LIGO data that have spectral line structures, such as the pernicious 60 Hz power-line and its harmonics, or lines associated with the suspension systems of the LIGO test masses (here, around ~340 Hz). You also don't appreciate that these searches also look at frequency derivatives -- how the 'monochromatic' signal may be slightly quasi-chromatic over time. It would be great to see the frequency evolution. 

Shiny is a great tool to interactively select a target to look at, and be able to explore the various spectral structures, as well as the potential frequency evolution. Here, you simply select the target in the drop-down list then select a region of interest on the main plot to zoom in on. The three other graphs update with a zoom of the frequency plot, as well as a display of the frequency derivatives.

For an overview of the science behind it, please visit http://www.ligo.org/science/Publication-S6DirectedSNR/index.php 


For more specifics, see the following paper (ApJ): http://iopscience.iop.org/article/10.1088/0004-637X/813/1/39/meta

Or browse it on the arXiv: https://arxiv.org/abs/1412.5942
