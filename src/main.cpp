#include <iostream>
#include <string.h>

#include "moduleMesh.h"

using namespace std;

int main (int argc, const char * argv[]) {
  meshC myMesh;
  
  bool ascii=0;
  
  cout << "programme de test" << endl;
  
  myMesh.nlog=6;
  cout << "myMesh.nlog="<< myMesh.nlog << endl;
  
  cout << "Fichier (Tetra): " ;
  //fgets(&myMesh.mesh_name[0],256,stdin) equivalent à fgets(myMesh.mesh_name,256,stdin);
  fgets(myMesh.mesh_name,256,stdin);
  int lenght=strlen(myMesh.mesh_name);
    cout << "lenght="<< lenght << endl;

  myMesh.mesh_name[lenght-1]='\0'; // end of the string
  
  // Pour le fun
  if( strcmp(myMesh.mesh_name,"Tetra")==0  ){
    cout << "Bon Choix !"<< endl;
  }else{
    cout << "Mauvais Choix ! "<< endl;
  }
  
  myMesh.nVert=0;
  myMesh.nSol =0;
  myMesh.nFld =0;     
  myMesh.solu =NULL;
  myMesh.hexas=NULL;
  myMesh.quadr=NULL;
  myMesh.trian=NULL;
  myMesh.edges=NULL;
  myMesh.solu =NULL;
  myMesh.norms=NULL;
  
  cout << "myMesh.mesh_name=" << myMesh.mesh_name << endl;
  
  cout << "Leaving main" << endl;
  read_mesh(&myMesh);
  cout << "Back in main" << endl;
  
  cout << "\nmyMesh.geo=" << myMesh.geo << endl;
  
  int ad=0;
  cout << "\nmyMesh.nVert=" << myMesh.nVert << endl;
  for( int iVert=0; iVert<myMesh.nVert; ++iVert ) {
    cout << "myMesh.vertx" << iVert << ": ";
    for(int iGeo=0; iGeo<3; ++iGeo){
      cout << myMesh.vertx[ad] << " " ;
      ad++;
    }
    cout << "mark=" << myMesh.mark[iVert] << endl;
  }

  ad=0;
  cout << "\nmyMesh.nT4=" << myMesh.nT4 << endl;
  for( int iCell=0; iCell<myMesh.nT4; ++iCell ) {
    cout << "myMesh.tetra" << iCell << ": ";
    for(int ker=0; ker<5; ++ker){
      cout << myMesh.tetra[ad] << " " ;
      ad++;
    }
    cout << endl;
  }

  ad=0;
  cout << "\nmyMesh.nT3=" << myMesh.nT3 << endl;
  for( int iCell=0; iCell<myMesh.nT3; ++iCell ) {
    cout << "myMesh.trian" << iCell << ": ";
    for(int ker=0; ker<4; ++ker){
      cout << myMesh.trian[ad] << " " ;
      ad++;
    }
    cout << endl;
  }


   
  return 0;

  cflAnalyser(&myMesh ,&ascii);
  cout << "Analyse CFL terminee ascii:" << ascii  << endl;
  
  cout << "Les tests sont termines" << endl;


  return 0;
}

