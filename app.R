library(shiny)
library("devtools")
library("ROpenCVLite")
#installOpenCV(batch = T)

#installOpenCV()
library("Rvision")
library("magick")
library("paws")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Webcam"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    my_stream <- stream(0)   # 0 will start your default webcam in general.
    my_selfie <- readNext(my_stream)
    #plot(my_selfie)
    release(my_stream)
    write.Image(my_selfie, "selfie.png")


    grp.photo = "selfie.png"

    # Read the photo using magick
    img = image_read(grp.photo)

    # Get basic informatino about the photo that will be useful for annotating
    inf = image_info(img)

    # Detect the faces in the image and pull all attributes associated with faces
    o = svc$detect_faces(Image=list(Bytes=grp.photo), Attributes="ALL")


    # Just get the face details
    all_faces = o$FaceDetails
    length(all_faces)

    # Loop through the faces, one by one. For each face, draw a rectangle around it, add the kid's name, and emotions

    # Duplicate the original image to have something to annotate and output
    new.img = img

    for(face in all_faces) {

        # Prepare a label that collapses across the emotions data provided by rekognition. Give the type of
        # emotion and the confidence that AWS has in its expression.
        emo.label = ""
        for(emo in face$Emotions) {
            emo.label = paste(emo.label,emo$Type, " = ", round(emo$Confidence, 2), "\n", sep="")
        }

        # Identify the coordinates of the face. Note that AWS returns percentage values of the total image size. This is
        # why the image info object above is needed
        box = face$BoundingBox
        image_width=inf$width
        image_height=inf$height
        x1 = box$Left*image_width
        y1 = box$Top*image_height
        x2 = x1 + box$Width*image_width
        y2 = y1 + box$Height*image_height

        # Create a subset image in memory that is just cropped around the focal face
        img.crop = image_crop(img, paste(box$Width*image_width,"x",box$Height*image_height,"+",x1,"+",y1, sep=""))
        img.crop = image_write(img.crop, path = NULL, format = "png")

        # Search in a specified collection to see if we can label the identity of the face is in this crop
        o = svc$search_faces_by_image(CollectionId="family-r",Image=list(Bytes=img.crop), FaceMatchThreshold=70)

        # Create a graphics device version of the larger photo that we can annotate
        new.img = image_draw(new.img)

        # If the face matches something in the collection, then add the name to the image
        if(length(o$FaceMatches) > 0) {
            faceName = o$FaceMatches[[1]]$Face$ExternalImageId
            faceConfidence = round(o$FaceMatches[[1]]$Face$Confidence,3)
            print(paste("Detected: ",faceName, sep=""))
            # Annotate with the name of the person
            text(x=x1+(box$Width*image_width)/2, y=y1,faceName, adj=0.5, cex=3, col="green")
        }

        # Draw a rectangle around the face
        rect(x1,y1,x2,y2, border="green", lty="dashed", lwd=5)

        # Annotate the photo with the emotions information
        text(x=x1+(box$Width*image_width)/2, y=y1+50,emo.label, pos=1, cex=1.5, col="red")

        dev.off()
    }

    # Write the image out to file
    #image_write(new.img, path="/Users/dre/Downloads/rekon/annotated_image2.png", format="png")



    output$distPlot <- renderPlot({
        plot(new.img)

    })
}

# Run the application
shinyApp(ui = ui, server = server)
