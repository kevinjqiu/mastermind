package mastermind

import "math"

func cartesianProduct(sets []string) []string {
	// Transliterated from:
	// http://stackoverflow.com/questions/2419370/how-can-i-compute-a-cartesian-product-iteratively
	var (
		i      int
		j      int
		item   []rune
		result []string
	)

	for {
		item = []rune{}
		j = i
		for _, str := range sets {
			item = append(item, rune(str[int(math.Mod(float64(j), float64(len(str))))]))
			j /= len(str)
		}
		if j > 0 {
			break
		}
		result = append(result, string(item))
		i += 1
	}

	return result
}
