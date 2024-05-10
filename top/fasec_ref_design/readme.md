# FASEC Reference design

More info on this board at https://ohwr.org/project/fasec/wikis/home

Construct the project as follows:
```
make
```

The script was orignally created with Vivado v2016.4 and last tested with v2021.1.

The build has not been automated, proceed as follows:
- open the Vivado project *.xpr
- click right on fasec_ref_design
  - Create HDL Wrapper
  - Generate Output Products, select 'Out of context per IP' 
- launch synthesis & implementation
