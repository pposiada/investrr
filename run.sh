#!/bin/bash

# Exit on error
set -e

IMAGE_NAME="investrr"
WORKSPACE_DIR="$(pwd)"

show_help() {
    echo "Usage: ./run.sh [command] [args]"
    echo ""
    echo "Commands:"
    echo "  build                  Build the Docker image with R and all packages"
    echo "  ui                     Run the Shiny dashboard (accessible at http://localhost:8080)"
    echo "  script <path_to_R>     Run a specific R script (e.g., ./run.sh script analyze_PHGE.R)"
    echo "  shell                  Start an interactive bash session inside the R environment"
    echo "  help                   Show this help message"
    echo ""
}

if [ -z "$1" ]; then
    show_help
    exit 1
fi

case "$1" in
    build)
        echo "Building Docker image '${IMAGE_NAME}'..."
        docker build -t "${IMAGE_NAME}" .
        echo "Image built successfully!"
        ;;
    ui)
        echo "Starting Shiny Dashboard on http://localhost:8080..."
        echo "Press Ctrl+C to stop the dashboard."
        docker run --rm -it \
            -p 8080:8080 \
            -v "${WORKSPACE_DIR}:/app" \
            -w /app \
            "${IMAGE_NAME}" \
            R -e "shiny::runApp('main/cloud_app/app.R', host='0.0.0.0', port=8080)"
        ;;
    script)
        if [ -z "$2" ]; then
            echo "Error: Please specify the R script to run."
            echo "Example: ./run.sh script analyze_PHGE.R"
            exit 1
        fi
        SCRIPT_PATH="$2"
        echo "Running R script: ${SCRIPT_PATH}..."
        docker run --rm -it \
            -v "${WORKSPACE_DIR}:/app" \
            -w /app \
            "${IMAGE_NAME}" \
            Rscript "${SCRIPT_PATH}"
        ;;
    shell)
        echo "Starting interactive bash shell..."
        docker run --rm -it \
            -v "${WORKSPACE_DIR}:/app" \
            -w /app \
            "${IMAGE_NAME}" \
            /bin/bash
        ;;
    help|*)
        show_help
        ;;
esac
