package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math/big"

	"github.com/elimsc/rescue_go/ff"
)

// =============== constants =======================

const (
	STATE_WIDTH  = 4
	NUM_ROUNDS   = 14
	CYCLE_LENGTH = 16
	M            = "21888242871839275222246405745257275088548364400416034343698204186575808495617"
)

var (
	ALPHA     *big.Int
	INV_ALPHA *big.Int
	MDS       []ff.Element
	INV_MDS   []ff.Element
	ARK       [][]ff.Element
)

func init_mds_and_inv() {
	var MDS1 = []string{
		"340282366920938463463374557953744960808",
		"1080",
		"340282366920938463463374557953744961147",
		"40",
		"340282366920938463463374557953744932377",
		"42471",
		"340282366920938463463374557953744947017",
		"1210",
		"340282366920938463463374557953744079447",
		"1277640",
		"340282366920938463463374557953744532108",
		"33880",
		"340282366920938463463374557953720263017",
		"35708310",
		"340282366920938463463374557953733025977",
		"925771",
	}
	// init MDS
	var res1 = make([]ff.Element, len(MDS1))
	for i := 0; i < len(MDS1); i++ {
		var e ff.Element
		e.SetString(MDS1[i])
		res1[i] = e
	}
	MDS = res1

	var INVMDS = []string{
		"8138170836863241576224485011528071696597514844813932685682467310048924000112",
		"15023268392528856731693853468825060661086777406192987399223888572791203892748",
		"1980175489278737600274358498818237879256902576493681414188073895027830875703",
		"8756200059540376130432449565783601621997062399729103381933846120505265488339",
		"16415234008013340919404158635873254488610404425650955688953403508714829254546",
		"2515198299285178712439148952208233690924836408463100454650766497382492288404",
		"8623445935382323389809538788821727509812361071416440105103662246252688475460",
		"6343936535530369016972300168051756169590655321699208632320443646023214238492",
		"2007670392327233629894204216549307120463647875745446317569268293489702248032",
		"10248557288501346965979756711776063421273445366521880775533328457954258686501",
		"17591949588791860285664058086819073259117875687872063074432929382660007495532",
		"4049637508590771157087127529810528058083288297090314713492749764269255826837",
		"11045202771509156595458220854851693679658514384010503021894589912363938677635",
		"13257731967726926336573607951255115719683843886438854251835078218096215592925",
		"795728467201634560072953459660312758308319425926029302791529391091597623165",
		"8799151571773494546520364279187849701287579530854318304507078376821472363177",
	}

	// init INV_MDS
	var res2 = make([]ff.Element, len(INVMDS))
	for i := 0; i < len(INVMDS); i++ {
		var e ff.Element
		e.SetString(INVMDS[i])
		res2[i] = e
	}
	INV_MDS = res2
}

func init_alpha_and_inv() {
	m := big.NewInt(0)
	m.SetString(M, 10)
	alpha, invAlpha := getAlphaAndInvAlpha(m)
	ALPHA = alpha
	INV_ALPHA = invAlpha
}

// input: module, ouput: alpha, inv_alpha
func getAlphaAndInvAlpha(m *big.Int) (*big.Int, *big.Int) {
	one := big.NewInt(1)
	m1 := big.NewInt(1)
	m1.Sub(m, m1) // m1 = m - 1
	var alpha = big.NewInt(3)
	for alpha.Cmp(m1) < 0 {
		if gcd(alpha, m1).Cmp(one) == 0 {
			break
		}
		alpha.Add(alpha, one)
	}
	invAlpha, _ := exgcd(alpha, m1)
	invAlpha.Mod(invAlpha, m1)
	return alpha, invAlpha
}

// https://github.com/spetacular/weekly-learning/blob/master/ch3/gcd.go
//辗转相除法求最大公约数 a > b
func gcd(a *big.Int, b *big.Int) *big.Int {
	var z = big.NewInt(0)
	z.GCD(nil, nil, a, b)
	return z
}

//扩展欧几里德算法 a > b
func exgcd(a *big.Int, b *big.Int) (*big.Int, *big.Int) {
	var zero = big.NewInt(0)
	var one = big.NewInt(1)
	if b.Cmp(zero) == 0 {
		return one, zero
	}
	var modRes = big.NewInt(0)
	var divRes = big.NewInt(0)
	x, y := exgcd(b, modRes.Mod(a, b))
	divRes.Div(a, b)      // a / b
	divRes.Mul(divRes, y) // (a / b) * y
	x, y = y, x.Sub(x, divRes)
	return x, y
}

func init_ark() {
	var ARK1 = [][]string{
		{
			"252629594110556276281235816992330349983",
			"121163867507455621442731872354015891839",
			"244623479936175870778515556108748234900",
			"181999122442017949289616572388308120964",
			"130035663054758320517176088024859935575",
			"274932696133623013607933255959111946013",
			"130096286077538976127585373664362805864",
			"209506446014122131232133742654202790201",
		},
		{
			"51912929769931267810162308005565017268",
			"202610584823002946089528994694473145326",
			"295992101426532309592836871256175669136",
			"313404555247438968545340310449654540090",
			"137671644572045862038757754124537020379",
			"29113322527929260506148183779738829778",
			"98634637270536166954048957710629281939",
			"90484051915535813802492401077197602516",
		},
		{
			"193753019093186599897082621380539177732",
			"88328997664086495053801384396180288832",
			"134379598544046716907663161480793367313",
			"50911186425769400405474055284903795891",
			"12945394282446072785093894845750344239",
			"110650301505380365788620562912149942995",
			"154214463184362737046953674082326221874",
			"306646039504788072647764955304698381135",
		},
		{
			"279745705918489041552127329708931301079",
			"111293612078035530300709391234153848359",
			"18110020378502034462498434861690576309",
			"41797883582559360517115865611622162330",
			"333888808893608021579859508112201825908",
			"291192643991850989562610634125476905625",
			"115042354025120848770557866862388897952",
			"281483497320099569269754505499721335457",
		},
		{
			"172898111753678285350206449646444309824",
			"202661860135906394577472615378659980424",
			"141885268042225970011312316000526746741",
			"270195331267041521741794476882482499817",
			"196457080224171120865903216527675657315",
			"56730777565482395039564396246195716949",
			"4886253806084919544862202000090732791",
			"147384194551383352824518757380733021990",
		},
		{
			"119476237236248181092343711369608370324",
			"182869361251406039022577235058473348729",
			"45308522364899994411952744852450066909",
			"15438528253368638146901598290564135576",
			"130060283207960095436997328133261743365",
			"83953475955438079154228277940680487556",
			"328659226769709797512044291035930357326",
			"228749522131871685132212950281473676382",
		},
		{
			"46194972462682851176957413491161426658",
			"296333983305826854863835978241833143471",
			"138957733159616849361016139528307260698",
			"67842086763518777676559492559456199109",
			"45580040156133202522383315452912604930",
			"67567837934606680937620346425373752595",
			"202860989528104560171546683198384659325",
			"22630500510153322451285114937258973361",
		},
		{
			"324160761097464842200838878419866223614",
			"338466547889555546143667391979278153877",
			"189171173535649401433078628567098769571",
			"162173266902020502126600904559755837464",
			"136209703129442038834374731074825683052",
			"61998071517031804812562190829480056772",
			"307309080039351604461536918194634835054",
			"26708622949278137915061761772299784349",
		},
		{
			"129516553661717764361826568456881002617",
			"224023580754958002183324313900177991825",
			"17590440203644538688189654586240082513",
			"135610063062379124269847491297867667710",
			"146865534517067293442442506551295645352",
			"238139104484181583196227119098779158429",
			"39300761479713744892853256947725570060",
			"54114440355764484955231402374312070440",
		},
		{
			"222758070305343916663075833184045878425",
			"323840793618712078836672915700599856701",
			"103586087979277053032666296091805459741",
			"160263698024385270625527195046420579470",
			"76620453913654705501329735586535761337",
			"117793948142462197480091377165008040465",
			"86998218841589258723143213495722487114",
			"203188618662906890442620821687773659689",
		},
		{
			"313098786815741054633864043424353402357",
			"133085673687338880872979866135939079867",
			"219888424885634764555580944265544343421",
			"5893221169005427793512575133564978746",
			"123830602624063632344313821515642988189",
			"99030942908036387138287682010525589136",
			"181549003357535890945363082242256699137",
			"152424978799328476472358562493335008209",
		},
		{
			"274481943862544603168725464029979191673",
			"4975004592976331754728718693838357226",
			"101850445399221640701542169338886750079",
			"230325699922192981509673754024218912397",
			"50419227750575087142720761582056939006",
			"112444234528764731925178653200320603078",
			"312169855609816651638877239277948636598",
			"204255114617024487729019111502542629940",
		},
		{
			"95797476952346525817251811755749179939",
			"306977388944722094681694167558392710189",
			"300754874465668732709232449646112602172",
			"25567836410351071106804347269705784680",
			"129659188855548935155840545784705385753",
			"228441586459539470069565041053012869566",
			"178382533299631576605259357906020320778",
			"274458637266680353971597477639962034316",
		},
		{
			"280059913840028448065185235205261648486",
			"246537412674731137211182698562269717969",
			"259930078572522349821084822750913159564",
			"186061633995391650657311511040160727356",
			"179777566992900315528995607912777709520",
			"209753365793154515863736129686836743468",
			"270445008049478596978645420017585428243",
			"70998387591825316724846035292940615733",
		},
	}
	n1 := len(ARK1)
	n2 := len(ARK1[0])

	var res = make([][]ff.Element, n1)
	for i := 0; i < n1; i++ {
		res[i] = make([]ff.Element, n2)
	}

	for i := 0; i < n1; i++ {
		for j := 0; j < n2; j++ {
			var e ff.Element
			e.SetString(ARK1[i][j])
			res[i][j] = e
		}
	}
	ARK = res
}

// ================ cipher rounds =================

func apply_sbox(state []ff.Element) {
	for i := 0; i < len(state); i++ {
		state[i].Exp(state[i], ALPHA)
	}
}

func apply_inv_sbox(state []ff.Element) {
	for i := 0; i < len(state); i++ {
		state[i].Exp(state[i], INV_ALPHA)
	}
}

func apply_mds(state []ff.Element) {
	n := len(state)
	var temp = make([]ff.Element, n)
	var result = make([]ff.Element, n)
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			temp[j].Mul(&MDS[i*n+j], &state[j])
		}
		for j := 0; j < n; j++ {
			result[i].Add(&result[i], &temp[j])
		}
	}
	copy(state, result)
}

func apply_inv_mds(state []ff.Element) {
	n := len(state)
	var temp = make([]ff.Element, n)
	var result = make([]ff.Element, n)
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			temp[j].Mul(&INV_MDS[i*n+j], &state[j])
		}
		for j := 0; j < n; j++ {
			result[i].Add(&result[i], &temp[j])
		}
	}
	copy(state, result)
}

func add_constants(state, ark []ff.Element) {
	for i := 0; i < len(state); i++ {
		state[i].Add(&state[i], &ark[i])
	}
}

func round_step1(state, ark []ff.Element) {
	apply_sbox(state)
	apply_mds(state)
	add_constants(state, ark)
}

func round_step2(state, ark []ff.Element) {
	apply_inv_sbox(state)
	apply_mds(state)
	add_constants(state, ark)
}

func inv_round_step1(state, ark []ff.Element) {
	for i := 0; i < len(state); i++ {
		state[i].Sub(&state[i], &ark[i])
	}

	apply_inv_mds(state)
	apply_inv_sbox(state)
}

func inv_round_step2(state, ark []ff.Element) {
	for i := 0; i < len(state); i++ {
		state[i].Sub(&state[i], &ark[i])
	}

	apply_inv_mds(state)
	apply_sbox(state)
}

func expand_key(key []ff.Element) [][]ff.Element {
	var n = len(key)
	// don't change the value of key
	var key1 = make([]ff.Element, n)
	copy(key1, key)
	var res [][]ff.Element // len: 2 * NUM_ROUNDS
	for i := 0; i < NUM_ROUNDS; i++ {
		ark1 := ARK[i][:n]
		ark2 := ARK[i][n:]

		round_step1(key1, ark1)
		res = append(res, append([]ff.Element{}, key1...))

		round_step1(key1, ark2)
		res = append(res, append([]ff.Element{}, key1...))
	}
	return res
}

func apply_cipher_round(key_state, data_state []ff.Element, step int) {
	var idx = step % CYCLE_LENGTH
	n := len(data_state)
	if len(key_state) != n {
		panic("len(data_state) must equals to len(key_state)")
	}
	ark1 := ARK[idx][:n]
	ark2 := ARK[idx][n:]

	round_step1(key_state, ark1)
	round_step1(data_state, key_state)

	round_step2(key_state, ark2)
	round_step2(data_state, key_state)

}

func blockCipher(key_state, data_state, iv []ff.Element) [2][15][4]string {
	add_constants(data_state, iv)
	var midState [2][15][4]string
	for i := 0; i < 4; i++ {
		midState[0][0][i] = key_state[i].String()
		midState[1][0][i] = data_state[i].String()
	}
	for i := 0; i < NUM_ROUNDS; i++ {
		apply_cipher_round(key_state, data_state, i)
		for j := 0; j < 4; j++ {
			midState[0][i+1][j] = key_state[j].String()
			midState[1][i+1][j] = data_state[j].String()
		}
	}
	// check mid state
	checkMidState(midState)

	return midState

	// midBytes, err := json.Marshal(midState)
	// if err != nil {
	// 	panic(err)
	// }
	// ioutil.WriteFile(filename, midBytes, 0644)
}

// val1: [2]ff.Element, val2: [2]ff.Element
// out: [2]ff.Element
func hash(val1 []ff.Element, val2 []ff.Element) [2][15][4]string {
	if len(val1) != 2 || len(val2) != 2 {
		panic("hash state must be two")
	}
	var state = make([]ff.Element, 4)
	for i := 0; i < 2; i++ {
		state[i].Add(&val1[i], &val2[i])
	}
	var zeroVec = newElementVec([]string{"0", "0", "0", "0"})
	mid := blockCipher(zeroVec, state, zeroVec)
	copy(val1, state)
	return mid
}

func checkMidState(midState [2][15][4]string) {
	for j := 0; j < NUM_ROUNDS; j++ {
		key_str1 := midState[0][j]
		key_str3 := midState[0][j+1]
		data_str1 := midState[1][j]
		data_str3 := midState[1][j+1]

		var key1, key3, data1, data3 []ff.Element
		for i := 0; i < 4; i++ {
			var e1 ff.Element
			e1.SetString(key_str1[i])
			key1 = append(key1, e1)

			var e2 ff.Element
			e2.SetString(key_str3[i])
			key3 = append(key3, e2)

			var e3 ff.Element
			e3.SetString(data_str1[i])
			data1 = append(data1, e3)

			var e4 ff.Element
			e4.SetString(data_str3[i])
			data3 = append(data3, e4)

		}
		ark1 := append([]ff.Element{}, ARK[j][0:4]...)
		ark2 := append([]ff.Element{}, ARK[j][4:8]...)
		key2 := append([]ff.Element{}, key1...)
		key3Temp := append([]ff.Element{}, key3...)
		round_step1(key2, ark1)
		inv_round_step2(key3Temp, ark2)
		for i := 0; i < 4; i++ {
			if key2[i].Cmp(&key3Temp[i]) != 0 {
				panic("not equal")
			}
		}
		round_step1(data1, key2)
		inv_round_step2(data3, key3)
		for i := 0; i < 4; i++ {
			if data1[i].Cmp(&data3[i]) != 0 {
				panic("not equal")
			}
		}
	}
}

func newElementVec(from []string) []ff.Element {
	var res = make([]ff.Element, len(from))
	for i := 0; i < len(from); i++ {
		var e ff.Element
		e.SetString(from[i])
		res[i] = e
	}
	return res
}

func printElementVec(vec []ff.Element) {
	fmt.Println("[")
	for i := 0; i < len(vec); i++ {
		fmt.Println(vec[i].String(), ",")
	}
	fmt.Println("]")
}

func hashAndEnc(key, data []ff.Element) {
	var zeroVec = newElementVec([]string{"0", "0", "0", "0"})
	var state [][15][4]string

	key1 := append([]ff.Element{}, key...)
	data1 := append([]ff.Element{}, data...)
	mid := blockCipher(key1, data1, zeroVec)
	state = append(state, mid[0:2]...)

	mid1 := hash(key[0:2], key[2:4])
	mid2 := hash(data[0:2], data[2:4])

	zero_state1 := mid1[0]
	zero_state2 := mid2[0]

	for i := 0; i < 15; i++ {
		for j := 0; j < 4; j++ {
			if zero_state1[i][j] != zero_state2[i][j] {
				panic("should equal")
			}
		}
	}

	state = append(state, mid1[0:2]...)
	state = append(state, mid2[1])

	stateBytes, err := json.Marshal(state)
	if err != nil {
		panic(err)
	}
	err = ioutil.WriteFile("./mid.json", stateBytes, 0644)
	if err != nil {
		panic(err)
	}

	printElementVec(data1)
	printElementVec(key[0:2])
	printElementVec(data[0:2])

}

func main() {
	init_alpha_and_inv()
	init_mds_and_inv()
	init_ark()

	// var zeroVec = newElementVec([]string{"0", "0", "0", "0"})
	var data = newElementVec([]string{"1", "2", "3", "4"})
	var key = newElementVec([]string{"1", "2", "3", "4"})
	hashAndEnc(key, data)

}
