# rescue cipher

rescue cipher over p = `21888242871839275222246405745257275088548364400416034343698204186575808495617`([ALT_BN128 curve supported by Ethereum](https://zokrates.github.io/language/types.html#field))

references: 
- https://github.com/KULeuven-COSIC/Marvellous
- https://github.com/novifinancial/winterfell/blob/main/examples/src/rescue/rescue.rs

finite field: https://github.com/ConsenSys/goff
```
goff -m 21888242871839275222246405745257275088548364400416034343698204186575808495617 -o ./ff/ -p ff -e Element
```

TODO:
mds和inv_mds根据m自动生成