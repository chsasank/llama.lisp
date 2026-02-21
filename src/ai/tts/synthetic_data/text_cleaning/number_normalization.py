import json

kannada_englis_num = {
    b"\\u0ce6": "0",
    b"\\u0ce7": "1",
    b"\\u0ce8": "2",
    b"\\u0ce9": "3",
    b"\\u0cea": "4",
    b"\\u0ceb": "5",
    b"\\u0cec": "6",
    b"\\u0ced": "7",
    b"\\u0cee": "8",
    b"\\u0cef": "9",
}


direct_dict = {
    "0": "ಸೊನ್ನೆ",
    "1": "ಒಂದು",
    "2": "ಎರಡು",
    "3": "ಮೂರು",
    "4": "ನಾಲಕ್ಕು",
    "5": "ಐದು",
    "6": "ಆರು",
    "7": "ಯೋಳು",
    "8": "ಎಂಟು",
    "9": "ಒಂಬತ್ತು",
    "01": "ಒಂದು",
    "02": "ಎರಡು",
    "03": "ಮೂರು",
    "04": "ನಾಲಕ್ಕು",
    "05": "ಐದು",
    "06": "ಆರು",
    "07": "ಯೋಳು",
    "08": "ಎಂಟು",
    "09": "ಒಂಬತ್ತು",
    "10": "ಹತ್ತು",
    "11": "ಹನ್ನೊಂದು",
    "12": "ಹನ್ನೆರಡು",
    "13": "ಹದಿಮೂರು",
    "14": "ಹದಿನಾಲ್ಕು",
    "15": "ಹದಿನೈದು",
    "16": "ಹದಿನಾರು",
    "17": "ಹದಿನೇಳು",
    "18": "ಹದಿನೆಂಟು",
    "19": "ಹತ್ತೊಂಬತ್ತು",
    "20": "ಇಪ್ಪತ್ತು",
    "21": "ಇಪ್ಪತ್ತೊಂದು",
    "22": "ಇಪ್ಪತ್ತೆರಡು",
    "23": "ಇಪ್ಪತ್ತ್ಮೂರು",
    "24": "ಇಪ್ಪತ್ನಾಲ್ಕು",
    "25": "ಇಪ್ಪತ್ತೈದು",
    "26": "ಇಪ್ಪತ್ತಾರು",
    "27": "ಇಪ್ಪತ್ತೇಳು",
    "28": "ಇಪ್ಪತ್ತೆಂಟು",
    "29": "ಇಪ್ಪತ್ತೊಂಬತ್ತು",
    "30": "ಮೂವತ್ತು",
    "31": "ಮೂವತ್ತೊಂದು",
    "32": "ಮೂವತ್ತೆರಡು",
    "33": "ಮೂವತ್ತ್ಮೂರು",
    "34": "ಮೂವತ್ನಾಲ್ಕು",
    "35": "ಮೂವತ್ತೈದು",
    "36": "ಮೂವತ್ತಾರು",
    "37": "ಮೂವತ್ತೇಳು",
    "38": "ಮೂವತ್ತೆಂಟು",
    "39": "ಮೂವತ್ತೊಂಬತ್ತು",
    "40": "ನಲವತ್ತು",
    "41": "ನಲವತ್ತೊಂದು",
    "42": "ನಲವತ್ತೆರಡು",
    "43": "ನಲವತ್ತ್ಮೂರು",
    "44": "ನಲವತ್ನಾಲ್ಕು",
    "45": "ನಲವತ್ತೈದು",
    "46": "ನಲವತ್ತಾರು",
    "47": "ನಲವತ್ತೇಳು",
    "48": "ನಲವತ್ತೆಂಟು",
    "49": "ನಲವತ್ತೊಂಬತ್ತು",
    "50": "ಐವತ್ತು",
    "51": "ಐವತ್ತೊಂದು",
    "52": "ಐವತ್ತೆರಡು",
    "53": "ಐವತ್ತ್ಮೂರು",
    "54": "ಐವತ್ತ್ನಾಲ್ಕು",
    "55": "ಐವತ್ತೈದು",
    "56": "ಐವತ್ತಾರು",
    "57": "ಐವತ್ತೇಳು",
    "58": "ಐವತ್ತೆಂಟು",
    "59": "ಐವತ್ತೊಂಬತ್ತು",
    "60": "ಅರವತ್ತು",
    "61": "ಅರವತ್ತೊಂದು",
    "62": "ಅರವತ್ತೆರಡು",
    "63": "ಅರವತ್ಮೂರು ",
    "64": "ಅರವತ್ನಾಲ್ಕು ",
    "65": "ಅರವತ್ತೈದು",
    "66": "ಅರವತ್ತಾರು",
    "67": "ಅರವತ್ತೇಳು",
    "68": "ಅರವತ್ತೆಂಟು",
    "69": "ಅರವತ್ತೊಂಬತ್ತು",
    "70": "ಎಪ್ಪತ್ತು",
    "71": "ಎಪ್ಪತ್ತೊಂದು",
    "72": "ಎಪ್ಪತ್ತೆರಡು",
    "73": "ಎಪ್ಪತ್ತ್ಮೂರು",
    "74": "ಎಪ್ಪತ್ತ್ನಾಲ್ಕು",
    "75": "ಎಪ್ಪತ್ತೈದು",
    "76": "ಎಪ್ಪತ್ತಾರು",
    "77": "ಎಪ್ಪತ್ತೇಳು",
    "78": "ಎಪ್ಪತ್ತೆಂಟು",
    "79": "ಎಪ್ಪತ್ತೊಂಬತ್ತು",
    "80": "ಎಂಬತ್ತು",
    "81": "ಎಂಬತ್ತೊಂದು",
    "82": "ಎಂಬತ್ತೆರಡು",
    "83": "ಎಂಬತ್ತೆರಡು",
    "84": "ಎಂಬತ್ತ್ನಾಲ್ಕು",
    "85": "ಎಂಬತ್ತೈದು",
    "86": "ಎಂಬತ್ತಾರು",
    "87": "ಎಂಬತ್ತೇಳು",
    "88": "ಎಂಬತ್ತೆಂಟು",
    "89": "ಎಂಬತ್ತೊಂಬತ್ತು",
    "90": "ತೊಂಬತ್ತು",
    "91": "ತೊಂಬತ್ತೊಂದು",
    "92": "ತೊಂಬತ್ತೆರಡು",
    "93": "ತೊಂಬತ್ತ್ಮೂರು",
    "94": "ತೊಂಬತ್ತ್ನಾಲ್ಕು",
    "95": "ತೊಂಬತ್ತೈದು",
    "96": "ತೊಂಬತ್ತಾರು",
    "97": "ತೊಂಬತ್ತೇಳು",
    "98": "ತೊಂಬತ್ತೆಂಟು",
    "99": "ತೊಂಬತ್ತೊಂಬತ್ತು",
    "100": "ನೂರ",
    "00": "",
}

hundreds_digit = {
    "0": "",
    "1": "ನೂರ",
    "2": "ಇನ್ನೂರ",
    "3": "ಮುನ್ನೂರ",
    "4": "ನಾಲಕ್ಕುನ್ನೂರ",
    "5": "ಐನೂರ",
    "6": "ಆರುನೂರ",
    "7": "ಏಳುನೂರ",
    "8": "ಎಂಟುನೂರ",
    "9": "ಒಂಬೈನೂರ",
}
hundreds_dict = {
    "100": "ನೂರು",
    "200": "ಇನ್ನೂರು",
    "300": "ಮುನ್ನೂರು",
    "400": "ನಾನ್ನೂರು",
    "500": "ಐನೂರು",
    "600": "ಆರುನೂರು",
    "700": "ಏಳುನೂರು",
    "800": "ಎಂಟುನೂರು",
    "900": "ಒಂಬೈನೂರು",
}


def check_prefix_zero(input_number):
    if len(input_number) > len(input_number.lstrip("0")):
        return True
    else:
        return False


def two_one_digits(input_number, numerization_text=""):
    if len(input_number) > 0 and len(input_number) <= 2:
        numerization_text = numerization_text + " " + direct_dict[input_number]

    return numerization_text


def three_digits(input_number, numerization_text=""):
    truth_value = check_prefix_zero(input_number)
    if truth_value:
        input_number = input_number.lstrip("0")
        numerization_text = two_one_digits(input_number, numerization_text)

    elif (
        len(input_number) == 3 and numerization_text == "" and input_number[1:] == "00"
    ):
        numerization_text = hundreds_dict[input_number]

    elif len(input_number) == 3:
        numerization_text = numerization_text + " " + hundreds_digit[input_number[0]]
        numerization_text = two_one_digits(input_number[1:], numerization_text)

    return numerization_text.lstrip()


def five_four_digits(input_number, numerization_text=""):
    thousand = ["ಸಾವಿರ", "ಸಾವಿರದ"]
    truth_value = check_prefix_zero(input_number)
    if truth_value:
        input_number = input_number.lstrip("0")
        numerization_text = three_digits(input_number, numerization_text)

    elif (
        len(input_number) in [4, 5]
        and numerization_text == ""
        and input_number[-3:] == "000"
    ):
        numerization_text = two_one_digits(input_number[:-3]) + " " + thousand[0]

    elif len(input_number) in [4, 5]:
        numerization_text = (
            two_one_digits(input_number[:-3], numerization_text) + " " + thousand[1]
        )
        numerization_text = three_digits(input_number[-3:], numerization_text)

    return numerization_text


def seven_six_digits(input_number, numerization_text=""):
    lakh = ["ಲಕ್ಷ", "ಲಕ್ಷದ"]
    truth_value = check_prefix_zero(input_number)
    if truth_value:
        input_number = input_number.lstrip("0")
        numerization_text = five_four_digits(input_number, numerization_text)

    elif (
        len(input_number) in [6, 7]
        and numerization_text == ""
        and input_number[-5:] == "00000"
    ):
        numerization_text = (
            two_one_digits(input_number[:-5], numerization_text) + " " + lakh[0]
        )

    elif len(input_number) in [6, 7]:
        numerization_text = (
            two_one_digits(input_number[:-5], numerization_text) + " " + lakh[1]
        )
        numerization_text = five_four_digits(input_number[-5:], numerization_text)

    return numerization_text


def eight_digits(input_number, numerization_text=""):
    crore = ["ಕೋಟಿ"]
    truth_value = check_prefix_zero(input_number)
    if truth_value:
        input_number = input_number.lstrip("0")
        numerization_text = seven_six_digits(input_number, numerization_text)

    elif (
        len(input_number) in [8]
        and numerization_text == ""
        and input_number[-7:] == "0000000"
    ):
        numerization_text = (
            two_one_digits(input_number[:-7], numerization_text) + " " + crore[0]
        )

    elif len(input_number) in [8]:
        numerization_text = (
            two_one_digits(input_number[:-7], numerization_text) + " " + crore[0]
        )
        numerization_text = seven_six_digits(input_number[-7:], numerization_text)

    return numerization_text


def nine_plus_digits(input_number, numerization_text=""):
    if len(input_number) > 8:
        for i in input_number:
            numerization_text = two_one_digits(i, numerization_text)

        return numerization_text


def number_normalization(str_number, number_of_digits, numerization_text=""):
    output = ""
    new_str_number = ""
    str_number = str_number.lstrip("0")
    for idx, integers in enumerate(str_number):
        if integers.encode("unicode_escape") in kannada_englis_num:
            new_str_number = (
                new_str_number + kannada_englis_num[integers.encode("unicode_escape")]
            )
        else:
            new_str_number = new_str_number + str_number[idx]

    if number_of_digits < 9 and number_of_digits > 0:
        try:
            if number_of_digits in [1, 2]:
                output = two_one_digits(new_str_number, numerization_text)
            elif number_of_digits == 3:
                output = three_digits(new_str_number, numerization_text)
            elif number_of_digits in [4, 5]:
                output = five_four_digits(new_str_number, numerization_text)
            elif number_of_digits in [6, 7]:
                output = seven_six_digits(new_str_number, numerization_text)
            elif number_of_digits in [8]:
                output = eight_digits(new_str_number, numerization_text)
        except:
            print("error cause for str_number")
    else:
        output = nine_plus_digits(new_str_number)

    return output


if __name__ == "__main__":

    number = input("enter number to normalized : ")
    number_of_digits = len(str(number))
    str_number = str(number)
    output = number_normalization(str_number, number_of_digits)

    with open("kann_number.txt", "w", encoding="utf-8") as f:
        json.dump(output, f, ensure_ascii=False)
