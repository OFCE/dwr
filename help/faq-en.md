-  **Ok boomer (1/2), there is no climate change!**

In **Debtwatch**, there is nothing to prevent you from modifying the potential growth (loss of productivity, constraints on activities), the public spending requirements (compensation, investment) or an increase in compulsory taxation (the socialization of transition costs, behavioral taxation). How you set up these different hypotheses is up to you, knowing that there are many possible scenarios for the transition or for climate damage.

- **Ok, boomer (2/2), never heard of MMT or of cancelling the public debt?**

It's true, in **Debtwatch**, the public debt is not cancelled. The model is used to calculate the evolution of the debt, and we have stuck to that. On the other hand, it is possible to simulate a perpetual holding by the Central Bank, which would reduce the net interest paid on the debt. Be careful to specify the rate of the securities held by the Central Bank. If the latter is close to 0, the effect on the interest burden is limited.

- **It's a Keynesian model (it is the OFCE right?), which can only push in the direction of more spending, so...**

In **Debtwatch**, it is easy to neutralize the fiscal multipliers, although the economics literature is now clear on the empirical validity of non-zero multipliers, at least in the short run. With zero-level multipliers, many of the model's conclusions are preserved. You will also be able to explore how higher multipliers come to modify these conclusions and perhaps to write new pages in the General Theory.

- **How can I conduct a fiscal stimulus?**

In **Debtwatch**, it is easy to permanently increase government spending and relax the government debt target. How this scenario is linked to the growth gap, the inflation target or potential growth is up to you. We work on a way to interface simply a fiscal stimulus and the model, stay tuned !

- **In many reports the debt explodes, but in the simulations it never explodes. Is this a bug?**

In **Debtwatch**, instead of assuming a policy that doesn’t change for 10 years (as in the Arthuis report), we calculate the policies needed to reach the debt target set. For some parameters, you can see that it is not possible to reach an ambitious target in 20 years. Moreover, the Arthuis report, which is available in the predefined scenarios, presents simulations only for 10 years and therefore cannot be used to conclude much about medium-term debt issues. Formally, without a control (i.e. without a fiscal policy) the dynamic system of debt accumulation does not have a stable solution. With a fiscal policy, it almost always has one.

- **In this model, the debt can increase without consequence**

In **Debtwatch**, the instantaneous rate depends on the public debt. By increasing the parameter that defines its sensitivity [r_dette], it is possible to simulate a Greek-style scenario where rates rise sharply and the snowball effect demands a considerable fiscal effort. Combining this with the gap in the long-term growth rate, we can wind up with catastrophic scenarios where the increase in the debt leads to untenable fiscal choices.

- **I change potential growth [gpot], but it doesn't have much impact – why?**

In **Debtwatch**, government spending is aligned with GDP in value. If the economy grows more, we assume for example that wages will be higher including in the public sector (and vice versa). However, you could combine a reduction in growth with an increase in the share of public spending to represent less wealth creation and a slippage in public spending. On the other hand, if the debt target is the same and if things work as expected, the debt-to-GDP ratio will be unchanged in the long run!

- **I find that the interest rate and the debt burden are not increasing very fast!**

In **Debtwatch**, the sovereign rate is a weighted average of the rates on past issues, as the maturity is relatively long. Even if the market rate increases, the transmission is slow at the weighted average rate. By playing with [r_mat], it is possible to accelerate this transmission, even if changing this parameter is not realistic: we would have to redeem past bonds early and get back into debt with short maturities!

- **The calculation is really quick – is it sure that it is performing all the calculations?**

Thank you! In the **Debtwatch** algorithm, first, the fiscal policy parameters are calculated and then the simulation. These simulations are repeated a large number of times by randomly sampling the perturbations. With a little bit of black magic (notably the recursion of the dynamic model is solved) and the tools that Imperial College has developed for COVID simulations since 2020, all our simulations can be run in a few seconds. Whenever possible, the result already calculated is reused.

- **It is taking time to calculate a simulation; can't we speed it up?**

Numerous calculations are performed in the **Debtwatch** algorithm. It's probably possible to speed this up further, especially by performing the minimisation of the loss function in C++ (or Julia) rather than partially in R. If you want to contribute, feel free to join us on Github.

- **I'm using a previously calculated simulation, but the results are not exactly the same – what's going on?**

In the **Debtwatch** algorithm, the simulations are stochastic. Due to considerations of efficiency as well as other reasons related to the dynamics of the system, reproducibility is limited by randomness. Nevertheless, unless you have counterexamples, the simulations should be close.
