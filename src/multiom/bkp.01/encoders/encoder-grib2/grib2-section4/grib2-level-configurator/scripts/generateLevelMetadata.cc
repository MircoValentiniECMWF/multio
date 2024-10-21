#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <optional>
#include <algorithm>
#include <cctype>

// Define the structure to hold the data for each row
struct LevelData {
    std::string typeOfLevel;
    std::string ID;
    int typeOfFirstFixedSurface;
    int typeOfSecondFixedSurface;
    std::optional<std::string> scaledValueOfFirstFixedSurface;
    std::optional<std::string> scaleFactorOfFirstFixedSurface;
    std::optional<std::string> scaledValueOfSecondFixedSurface;
    std::optional<std::string> scaleFactorOfSecondFixedSurface;
};

// Function to split a CSV line by comma
std::vector<std::string> splitCSVLine(const std::string& line) {
    std::vector<std::string> result;
    std::stringstream ss(line);
    std::string item;

    while (std::getline(ss, item, ',')) {
        result.push_back(item);
    }
    return result;
}

// Function to parse a string and return std::optional<std::string>
std::optional<std::string> parseOptionalString(const std::string& str) {
    if (str.empty() || str == "MISSING") {
        return std::nullopt;  // Missing value
    }
    return str;  // Value exists
}

// Function to load data from CSV file
std::vector<LevelData> loadCSV(const std::string& filename) {
    std::ifstream file(filename);
    std::string line;
    std::vector<LevelData> data;

    if (!file.is_open()) {
        std::cerr << "Error: Could not open file " << filename << std::endl;
        return data;
    }

    // Skip the header line
    std::getline(file, line);

    // Read each line of the file
    while (std::getline(file, line)) {
        auto columns = splitCSVLine(line);

        // Ensure we have exactly 7 columns
        if (columns.size() != 7) {
            std::cerr << "Error: Invalid number of columns in line: " << line << std::endl;
            continue;
        }

        // Parse the row and store in LevelData structure
        LevelData row;
	std::string str=columns[0];
        std::transform(str.begin(), str.end(), str.begin(), [](unsigned char c) {
          return std::toupper(c);
        });
	row.ID = str;    // Convert the string to uppercase
        row.typeOfLevel = columns[0];
        row.typeOfFirstFixedSurface = std::stoi(columns[1]);
        row.typeOfSecondFixedSurface = std::stoi(columns[2]);
        row.scaledValueOfFirstFixedSurface = parseOptionalString(columns[3]);
        row.scaleFactorOfFirstFixedSurface = parseOptionalString(columns[4]);
        row.scaledValueOfSecondFixedSurface = parseOptionalString(columns[5]);
        row.scaleFactorOfSecondFixedSurface = parseOptionalString(columns[6]);

        // Store the parsed row
        data.push_back(row);
    }

    file.close();
    return data;
}

// Function to print the data
void printData(const std::vector<LevelData>& data) {
    for (const auto& row : data) {
        std::cout << "TypeOfLevel: " << row.typeOfLevel << std::endl
                  << "  - TypeOfFirstFixedSurface: " << row.typeOfFirstFixedSurface << std::endl
                  << "  - TypeOfSecondFixedSurface: " << row.typeOfSecondFixedSurface << std::endl
                  << "  - ScaledValueOfFirstFixedSurface: " << (row.scaledValueOfFirstFixedSurface ? *row.scaledValueOfFirstFixedSurface : "N/A") << std::endl
                  << "  - ScaleFactorOfFirstFixedSurface: " << (row.scaleFactorOfFirstFixedSurface ? *row.scaleFactorOfFirstFixedSurface : "N/A") << std::endl
                  << "  - ScaledValueOfSecondFixedSurface: " << (row.scaledValueOfSecondFixedSurface ? *row.scaledValueOfSecondFixedSurface : "N/A") << std::endl
                  << "  - ScaleFactorOfSecondFixedSurface: " << (row.scaleFactorOfSecondFixedSurface ? *row.scaleFactorOfSecondFixedSurface : "N/A") << std::endl
                  << std::endl;
    }
}

std::string parse( std::string xxx ){
	return xxx;
}

// Function to print the data
void printCode(const LevelData& row) {

        std::cout << "  PP_LOG_INFO( 'TypeOfLevel: " << row.typeOfLevel << "' )" << std::endl;
	std::cout << "  IF ( OPT%USE_TYPE_OF_LEVEL ) THEN" << std::endl;
        std::cout << "    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'typeOfLevel', '" << row.typeOfLevel << "' )" << std::endl;
        std::cout << "    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'level', MSG%LEVELIST )" << std::endl;
	std::cout << "  ELSE" << std::endl;
        std::cout << "    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'TypeOfFirstFixedSurface', " << row.typeOfFirstFixedSurface << "_JPIB_K )" <<std::endl;
        std::cout << "    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'TypeOfSecondFixedSurface', " << row.typeOfSecondFixedSurface << "_JPIB_K )" << std::endl;
	if ( row.scaledValueOfFirstFixedSurface ) {
            std::cout << "    ! TODO PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'ScaledValueOfFirstFixedSurface', " << parse(*row.scaledValueOfFirstFixedSurface) << ")" << std::endl;
	}
	else
        {
            std::cout << "    PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'ScaledValueOfFirstFixedSurface' "  <<  ")" << std::endl;
	}

	if ( row.scaledValueOfSecondFixedSurface ) {
            std::cout << "    ! TODO PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'ScaledValueOfSecondFixedSurface', " << parse(*row.scaledValueOfSecondFixedSurface) << ")" << std::endl;
	}
	else
        {
            std::cout << "    PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'ScaledValueOfSecondFixedSurface' "  <<  ")" << std::endl;
	}
	if ( row.scaleFactorOfFirstFixedSurface ) {
            std::cout << "    ! TODO PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'ScaleFactorOfFirstFixedSurface', " << parse(*row.scaleFactorOfFirstFixedSurface) << ")" << std::endl;
	}
	else
        {
            std::cout << "    PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'ScaleFactorOfFirstFixedSurface' "  <<  ")" << std::endl;
	}
	if ( row.scaleFactorOfSecondFixedSurface ) {
            std::cout << "    ! TODO PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'ScaleFactorOfSecondFixedSurface', " << parse(*row.scaleFactorOfSecondFixedSurface) << ")" << std::endl;
	}
	else
        {
            std::cout << "    PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'ScaleFactorOfFirstFixedSurface' "  <<  ")" << std::endl;
	}
	std::cout << "  ENDIF" << std::endl;
	std::cout << std::endl << std::endl;
        // std::cout << "  - ScaleFactorOfFirstFixedSurface: " << (row.scaleFactorOfFirstFixedSurface ? *row.scaleFactorOfFirstFixedSurface : "N/A") << std::endl;
        // std::cout << "  - ScaledValueOfSecondFixedSurface: " << (row.scaledValueOfSecondFixedSurface ? *row.scaledValueOfSecondFixedSurface : "N/A") << std::endl;
        // std::cout << "  - ScaleFactorOfSecondFixedSurface: " << (row.scaleFactorOfSecondFixedSurface ? *row.scaleFactorOfSecondFixedSurface : "N/A") << std::endl;
}


int main( int argc, char* argv[] ) {
    // Replace with your CSV file path
    std::string filename = "typeOfLevel.csv";
    
    // Load CSV data
    std::vector<LevelData> data = loadCSV(filename);

    // If requested just print one
    if (argc > 1) {
        // Convert the first argument (after the program name) to a std::string
        std::string str = argv[1];  // argv[0] is the program name, argv[1] is the first argument
        std::transform(str.begin(), str.end(), str.begin(), [](unsigned char c) {
          return std::toupper(c);
        });

        for (const auto& row : data) {
	    if ( row.ID == str ) {
              printCode( row );
	    }
        }
    } else {
        // Print loaded data
        printData(data);
        for (const auto& row : data) {
            printCode( row );
        }
    }

    
    return 0;
}

