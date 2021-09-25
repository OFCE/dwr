---
output: 
  html_document: 
    theme: null
    self_contained: no
---

1.  $\Delta og = og_{lag} \times \Delta og_{-1} -og_{mce} \times og + og_{dep} \times ib_{dep} + og_{po} \times ib_{po} + \varepsilon$

2.  $\Delta \varepsilon = ogn_{ar} \times \varepsilon + \epsilon (\sim \mathcal{N}(0,ogn_{sigma}^2)$

3.  $\dot{ppib}{+1} = \dot{ppib} + inf_{lag} \times(\dot p - \dot p_{-1}) - inf_{mce} \times (\dot p - inf_{star} - inf_{og} \times og)$

4.  $pibpot_{+1} = (1+g_{pot}) \times pib_{pot}$

5.  $qpib = (1+og) \times pibpot$

6.  $vpib = qpib \times ppib$

7.  $\Delta dette = ddette +one_{off}$

8.  $d = dette / vpib$

9.  $ddette = -s_p - int + t_{aut} \times vpib$

10. $r_{inst} = g_{pot} + \dot{ppib} + ec + r_{dette} \times (d - d^{**})$

11. $r_{app+1} = 1/r_{mat} \times r_{inst} + (1-1/r_{mat}) \times r_{app}$

12. $dep = t_{dep} \times pib_{pot} \times ppib$

13. $po = t_{po} \times vpib$

14. $s_{p} = po - dep$

15. $ib_{fr} = tpo_{s^*} \times (spp - s^*) + tpo_{d^*} \times (d- d^*) + tpo_{og}\times og$

16. $(tpo_{d^*}, tpo_{og}, tpo_{s^*})_{tpo_.\in\mathcal{C}}\ \leftarrow min(E_0(loss))$

17. $loss = \Sigma og^2/(1+loss_{df})^t + loss_d \times \Sigma_{t>loss_t} (d-d^*)$

18. $ib_{spont,dep} = tx_{deriv,dep} \times index_{temps}\times t_{dep} + t_{dep} \times \Delta(\frac{ppib_{dep} \times pibpot_{dep}}{pibpot \times pibpot})$

19. $ib_{dep} = ib_{spont,dep} + (1-pcpo) \times ib_{sup}$

20. $\Delta t_{dep} = ib_{dep}/t_{dep}$

21. $ib_{spont,po} = - tx_{deriv,po} \times index_{temps} \times tpo$

22. $ib_{po} = ib_{spont,po} + pcpo \times ib_{sup}$

23. $\Delta t_{po} = - ib_{po}/t_{po}$

24. $ppib_{dep} = ppib_dep \times (1+ infdep_{star}*txppib + (1-infdep_{star})\times txppib_{dep} - infdep_{mce}\times (\frac{ppib_{dep}}{ppib}-1))$

25. $pibpot_{dep} = pibpot_{dep, -1} \times (1+ potdep_{star}\times g_{pot} + (1-potdep_{star}) \times txpibpot_{dep} - potdep_{mce}(\frac{pibpot_{dep}}{pibpot}-1))$

26. $\Delta tcho = - tcho_{okun} \times dog - tcho_{mce} \times (tcho+og-tvnairu)$

27. $tvnairu_{+1} = tvnairu - tvnairu_{mce} \times (tvnairu - nairu)$

$\Delta x = x_{+1} - x$
