
export YESOD_DEMO_LANG=KK
export YESOD_GCLOUD_PROJECT_ID=apps-430700
export APP_NAME=almamap
export INSTANCE_NAME=almamap
export DOCKER_IMAGE=asia-east1-docker.pkg.dev/${YESOD_GCLOUD_PROJECT_ID}/webapps/${APP_NAME}:demo-kk

docker rmi -f $DOCKER_IMAGE

docker build --no-cache \
       --build-arg YESOD_DEMO_LANG=$YESOD_DEMO_LANG \
       --build-arg YESOD_MAPBOX_PK=$YESOD_MAPBOX_PK \
       -t $DOCKER_IMAGE \
       -f Dockerfile .

docker push $DOCKER_IMAGE

gcloud run deploy ${INSTANCE_NAME} \
        --project=$YESOD_GCLOUD_PROJECT_ID \
        --region=asia-east1 \
        --min-instances=0 \
        --max-instances=1 \
        --cpu-throttling \
        --allow-unauthenticated \
        --image=$DOCKER_IMAGE

gcloud --project=${YESOD_GCLOUD_PROJECT_ID} artifacts docker images delete --quiet --delete-tags \
       asia-east1-docker.pkg.dev/${YESOD_GCLOUD_PROJECT_ID}/webapps/${APP_NAME}
